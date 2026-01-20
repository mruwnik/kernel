use std::{fmt, ops::Deref};
use std::rc::Rc;
use std::collections::HashSet;
use crate::errors::{ RuntimeError, ErrorTypes };
use crate::values::{ Value, ValueResult, CallResult, Symbol, CallResultType, is_val };
use crate::values::eval::eval;
use crate::values::numbers::Number;

use super::envs::{Env, EnvRef};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum CombinerType {
    Operative,
    Applicative,
}

pub type Func = &'static dyn Fn(Rc<Value>, EnvRef) -> CallResult;
type ValueFunc = &'static dyn Fn(Rc<Value>, Rc<Value>) -> ValueResult;
type Method = &'static dyn Fn(Rc<Value>) -> ValueResult;

/// The kind of combiner - either a primitive (built-in) or compound (user-defined)
#[derive(Clone)]
pub enum CombinerKind {
    /// Primitive combiner with a static function pointer
    Primitive {
        func: Func,
    },
    /// Compound combiner created by $vau
    Compound {
        static_env: EnvRef,
        formals: Rc<Value>,
        eformal: Rc<Value>,  // Symbol or #ignore
        body: Rc<Value>,
    },
    /// Wrapped combiner (for applicatives created by wrap)
    Wrapped {
        underlying: Rc<Value>,  // Must be a Combiner
    },
}

#[derive(Clone)]
pub struct Combiner {
    pub c_type: CombinerType,
    name: String,
    kind: CombinerKind,
}

impl fmt::Display for Combiner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            CombinerKind::Primitive { .. } => write!(f, "#[{}]", self.name),
            CombinerKind::Compound { formals, body, .. } => {
                write!(f, "#[compound {} {}]", formals, body)
            }
            CombinerKind::Wrapped { underlying } => write!(f, "#[wrapped {}]", underlying),
        }
    }
}

impl fmt::Debug for Combiner {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Combiner")
            .field("type", &self.c_type)
            .field("name", &self.name)
            .finish()
    }
}

impl PartialEq for Combiner {
    fn eq(&self, other: &Self) -> bool {
        // For now, combiners are equal if they have the same name and type
        // This is a simplification; full equality would need to compare kind
        self.name == other.name && self.c_type == other.c_type
    }
}

pub fn number_applicative(vals: Rc<Value>, func: &dyn Fn(Rc<Value>) -> ValueResult, symbol: impl Into<String>) -> CallResult {
    let res: Rc<Value> = func(vals.clone())?.into();
    match res.deref() {
        Value::Number(_) => res.as_val(),
        Value::Pair(_) => Value::cons(
            Value::make_symbol(symbol),
            res,
        )?.as_tail_call(),
        _ => RuntimeError::type_error("+ can only handle numbers and lists"),
    }
}

pub fn two_val_fn(params: Vec<Rc<Value>>, name: impl Into<String>, func: ValueFunc) -> ValueResult {
    match &params[..] {
        [val1, val2] => {
            func(val1.clone(), val2.clone())
        },
        _ => RuntimeError::type_error(format!("{} requires 2 arguments", name.into())),
    }
}


fn method(val: Rc<Value>, func: Method) -> ValueResult {
    func(val)?.car()
}

impl Combiner {
    fn new_primitive(name: impl Into<String>, func: Func, c_type: CombinerType) -> Rc<Value> {
        Rc::new(Value::Combiner(Combiner {
            c_type,
            name: name.into(),
            kind: CombinerKind::Primitive { func },
        }))
    }

    pub fn new_compound(
        static_env: EnvRef,
        formals: Rc<Value>,
        eformal: Rc<Value>,
        body: Rc<Value>,
    ) -> Rc<Value> {
        Rc::new(Value::Combiner(Combiner {
            c_type: CombinerType::Operative,
            name: "$vau".to_string(),
            kind: CombinerKind::Compound {
                static_env,
                formals,
                eformal,
                body,
            },
        }))
    }

    pub fn new_wrapped(underlying: Rc<Value>) -> Result<Rc<Value>, RuntimeError> {
        // Verify that underlying is a combiner
        if !matches!(underlying.deref(), Value::Combiner(_)) {
            return RuntimeError::type_error("wrap requires a combiner");
        }
        Ok(Rc::new(Value::Combiner(Combiner {
            c_type: CombinerType::Applicative,
            name: "wrapped".to_string(),
            kind: CombinerKind::Wrapped { underlying },
        })))
    }

    pub fn unwrap(&self) -> Result<Rc<Value>, RuntimeError> {
        if self.c_type != CombinerType::Applicative {
            return RuntimeError::type_error("unwrap requires an applicative");
        }
        match &self.kind {
            CombinerKind::Wrapped { underlying } => Ok(underlying.clone()),
            CombinerKind::Primitive { func } => {
                // Create an operative version of this primitive
                Ok(Rc::new(Value::Combiner(Combiner {
                    c_type: CombinerType::Operative,
                    name: format!("unwrapped-{}", self.name),
                    kind: CombinerKind::Primitive { func: *func },
                })))
            }
            CombinerKind::Compound { .. } => {
                // Compound applicatives shouldn't exist directly, but handle it
                RuntimeError::type_error("cannot unwrap compound applicative")
            }
        }
    }

    pub fn is_eq(self: &Self, other: &Self) -> Result<bool, RuntimeError> {
        // Two combiners are eq? only if they are the same object
        // For primitives, compare by name and type
        // For compound/wrapped, they're only eq? if same object (handled by pointer comparison in caller)
        Ok(self.name == other.name && self.c_type == other.c_type)
    }

    pub fn get_kind(&self) -> &CombinerKind {
        &self.kind
    }

    // proper underlying combiner implementations
    fn if_(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let env_val = Rc::new(Value::Env(env.clone()));
        let params = vals.operands()?;
        match &params[..] {
            [test, branch1, branch2] => {
                if eval(test.clone(), env_val.clone())?.is_true() {
                    branch1.eval(env_val)
                } else {
                    branch2.eval(env_val)
                }
            },
            _ => RuntimeError::type_error("$if requires 3 arguments"),
        }
    }

    fn eval(vals: Rc<Value>, _: EnvRef) -> CallResult {
        let params = vals.operands()?;
        match &params[..] {
            [val1, val2] => {
                val1.eval(val2.clone())
            },
            _ => RuntimeError::type_error(format!("eval requires 2 arguments")),
        }
    }

    fn define(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let env_val = Rc::new(Value::Env(env.clone()));
        let params = vals.operands()?;
        match &params[..] {
            [val1, val2] => {
                env_val.define(val1.clone(), val2.clone())?.as_val()
            },
            _ => RuntimeError::type_error(format!("$define! requires 2 arguments")),
        }
    }

    pub fn bind_ground(env: &EnvRef) {
        fn bind(env: &EnvRef, name: impl Into<String>, type_: CombinerType, func: Func) {
            let name = name.into();
            env.borrow_mut().bind(
                Symbol(name.clone()),
                Combiner::new_primitive(name, func, type_)
            );
        }

        // Core primitives
        bind(&env, "$if", CombinerType::Operative, &Combiner::if_);
        bind(&env, "$vau", CombinerType::Operative, &Combiner::vau);
        bind(&env, "$lambda", CombinerType::Operative, &Combiner::lambda);
        bind(&env, "$sequence", CombinerType::Operative, &Combiner::sequence);
        bind(&env, "wrap", CombinerType::Applicative, &Combiner::wrap);
        bind(&env, "unwrap", CombinerType::Applicative, &Combiner::unwrap_);
        bind(&env, "eq?", CombinerType::Applicative, &|vals, _| two_val_fn(vals.operands()?, "eq?", &Value::is_eq)?.as_val());
        bind(&env, "equal?", CombinerType::Applicative, &|vals, _| two_val_fn(vals.operands()?, "equal?", &Value::is_equal)?.as_val());
        bind(&env, "cons", CombinerType::Applicative, &|vals, _| two_val_fn(vals.operands()?, "cons", &Value::cons)?.as_val());
        bind(&env, "set-car!", CombinerType::Applicative, &|vals, _| two_val_fn(vals.operands()?, "set-car!", &Value::set_car)?.as_val());
        bind(&env, "set-cdr!", CombinerType::Applicative, &|vals, _| two_val_fn(vals.operands()?, "set-cdr!", &Value::set_cdr)?.as_val());
        bind(&env, "copy-es-immutable", CombinerType::Applicative, &|vals, _| method(vals, &Value::copy_es_immutable)?.as_val());
        bind(&env, "make-environment", CombinerType::Applicative, &|vals, _| Value::make_environment(vals)?.as_val());
        bind(&env, "eval", CombinerType::Applicative, &Combiner::eval);
        bind(&env, "$define!", CombinerType::Operative, &Combiner::define);

        // Boolean operations
        bind(&env, "not", CombinerType::Applicative, &Combiner::not);
        bind(&env, "$and?", CombinerType::Operative, &Combiner::and);
        bind(&env, "$or?", CombinerType::Operative, &Combiner::or);
        bind(&env, "and?", CombinerType::Applicative, &Combiner::and_applicative);
        bind(&env, "or?", CombinerType::Applicative, &Combiner::or_applicative);

        // Apply
        bind(&env, "apply", CombinerType::Applicative, &Combiner::apply);

        // Conditionals
        bind(&env, "$cond", CombinerType::Operative, &Combiner::cond);

        // Let bindings
        bind(&env, "$let", CombinerType::Operative, &Combiner::let_);
        bind(&env, "$let*", CombinerType::Operative, &Combiner::let_star);
        bind(&env, "$letrec", CombinerType::Operative, &Combiner::letrec);

        // Type checkers (all applicatives - they evaluate their arguments)
        bind(&env, "boolean?", CombinerType::Applicative, &|vals, _| Value::is_boolean(vals)?.as_val());
        bind(&env, "applicative?", CombinerType::Applicative, &|vals, _| Value::is_applicative(vals)?.as_val());
        bind(&env, "operative?", CombinerType::Applicative, &|vals, _| Value::is_operative(vals)?.as_val());
        bind(&env, "combiner?", CombinerType::Applicative, &|vals, _| Value::is_combiner(vals)?.as_val());
        bind(&env, "inert?", CombinerType::Applicative, &|vals, _| Value::is_inert(vals)?.as_val());
        bind(&env, "ignore?", CombinerType::Applicative, &|vals, _| Value::is_ignore(vals)?.as_val());
        bind(&env, "null?", CombinerType::Applicative, &|vals, _| Value::is_null(vals)?.as_val());
        bind(&env, "env?", CombinerType::Applicative, &|vals, _| Value::is_env(vals)?.as_val());
        bind(&env, "number?", CombinerType::Applicative, &|vals, _| Value::is_number(vals)?.as_val());
        bind(&env, "pair?", CombinerType::Applicative, &|vals, _| Value::is_pair(vals)?.as_val());
        bind(&env, "string?", CombinerType::Applicative, &|vals, _| Value::is_string(vals)?.as_val());
        bind(&env, "symbol?", CombinerType::Applicative, &|vals, _| Value::is_symbol(vals)?.as_val());

        // Library - arithmetic
        bind(&env, "+", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::add, "+"));
        bind(&env, "-", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::minus, "-"));
        bind(&env, "*", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::multiply, "*"));
        bind(&env, "/", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::divide, "/"));
        bind(&env, "mod", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::modulo, "mod"));

        // Comparisons (using Kernel-style names with ?)
        bind(&env, "<?", CombinerType::Applicative, &|vals, _| {
            two_val_fn(vals.operands()?, "<?", &|a, b| Number::less_than(&a, &b))?.as_val()
        });
        bind(&env, "<=?", CombinerType::Applicative, &|vals, _| {
            two_val_fn(vals.operands()?, "<=?", &|a, b| Number::less_than_or_equal(&a, &b))?.as_val()
        });
        bind(&env, ">?", CombinerType::Applicative, &|vals, _| {
            two_val_fn(vals.operands()?, ">?", &|a, b| Number::greater_than(&a, &b))?.as_val()
        });
        bind(&env, ">=?", CombinerType::Applicative, &|vals, _| {
            two_val_fn(vals.operands()?, ">=?", &|a, b| Number::greater_than_or_equal(&a, &b))?.as_val()
        });
        bind(&env, "=?", CombinerType::Applicative, &|vals, _| {
            two_val_fn(vals.operands()?, "=?", &|a, b| Number::numeric_equal(&a, &b))?.as_val()
        });

        // Library - list operations
        bind(&env, "car", CombinerType::Applicative, &|vals, _| vals.car()?.car()?.as_val());
        bind(&env, "cdr", CombinerType::Applicative, &|vals, _| vals.car()?.cdr()?.as_val());
        bind(&env, "list", CombinerType::Applicative, &|vals, _| vals.as_val());
    }

    // $vau operative: creates a compound operative
    // ($vau <formals> <eformal> <body>)
    fn vau(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        if params.len() < 2 {
            return RuntimeError::type_error("$vau requires at least formals and eformal");
        }

        let formals = params[0].clone();
        let eformal = params[1].clone();

        // Validate eformal: must be symbol or #ignore
        match eformal.deref() {
            Value::Symbol(_) | Value::Constant(crate::values::Constant::Ignore) => {}
            _ => return RuntimeError::type_error("$vau eformal must be a symbol or #ignore"),
        }

        // Validate formals and check for duplicates
        let mut seen_symbols: HashSet<String> = HashSet::new();
        validate_formals(&formals, &mut seen_symbols)?;

        // Check that eformal is not in formals
        if let Value::Symbol(Symbol(name)) = eformal.deref() {
            if seen_symbols.contains(name) {
                return RuntimeError::type_error("eformal cannot duplicate a symbol in formals");
            }
        }

        // Get body - for now just single expression, later we'll add $sequence
        let body = if params.len() == 2 {
            Value::make_inert()
        } else if params.len() == 3 {
            params[2].clone()
        } else {
            // Multiple body expressions - wrap in implicit $sequence
            // For now, just use the last one
            // TODO: implement proper $sequence handling
            params[params.len() - 1].clone()
        };

        // Make immutable copies of formals and body (if they're pairs)
        let formals_copy = if matches!(formals.deref(), Value::Pair(_)) {
            Value::copy_es_immutable(formals.clone())?
        } else {
            formals
        };
        let body_copy = if matches!(body.deref(), Value::Pair(_)) {
            Value::copy_es_immutable(body.clone())?
        } else {
            body
        };

        Combiner::new_compound(env, formals_copy, eformal, body_copy).as_val()
    }

    // wrap applicative: wraps a combiner to create an applicative
    fn wrap(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        match &params[..] {
            [combiner] => Combiner::new_wrapped(combiner.clone())?.as_val(),
            _ => RuntimeError::type_error("wrap requires exactly 1 argument"),
        }
    }

    // unwrap applicative: extracts the underlying combiner from an applicative
    fn unwrap_(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        match &params[..] {
            [val] => {
                if let Value::Combiner(c) = val.deref() {
                    c.unwrap()?.as_val()
                } else {
                    RuntimeError::type_error("unwrap requires an applicative")
                }
            }
            _ => RuntimeError::type_error("unwrap requires exactly 1 argument"),
        }
    }

    // $lambda operative: creates an applicative (wrapped operative with #ignore for eformal)
    // ($lambda <formals> . <body>) = (wrap ($vau <formals> #ignore . <body>))
    fn lambda(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        if params.is_empty() {
            return RuntimeError::type_error("$lambda requires at least formals");
        }

        let formals = params[0].clone();

        // Validate formals and check for duplicates
        let mut seen_symbols: HashSet<String> = HashSet::new();
        validate_formals(&formals, &mut seen_symbols)?;

        // Get body - for now just single expression, later we'll add $sequence
        let body = if params.len() == 1 {
            Value::make_inert()
        } else if params.len() == 2 {
            params[1].clone()
        } else {
            // Multiple body expressions - wrap in implicit $sequence
            // TODO: implement proper $sequence handling
            params[params.len() - 1].clone()
        };

        // Make immutable copies of formals and body (if they're pairs)
        let formals_copy = if matches!(formals.deref(), Value::Pair(_)) {
            Value::copy_es_immutable(formals.clone())?
        } else {
            formals
        };
        let body_copy = if matches!(body.deref(), Value::Pair(_)) {
            Value::copy_es_immutable(body.clone())?
        } else {
            body
        };

        // Create the compound operative with #ignore for eformal
        let operative = Combiner::new_compound(
            env,
            formals_copy,
            Value::make_const(crate::values::Constant::Ignore),
            body_copy,
        );

        // Wrap it to make an applicative
        Combiner::new_wrapped(operative)?.as_val()
    }

    // $sequence operative: evaluates operands in order, returns last result
    // ($sequence . <exprs>) evaluates each expr and returns the result of the last one
    fn sequence(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let exprs = vals.operands()?;
        if exprs.is_empty() {
            return Value::make_inert().as_val();
        }

        let env_val = Rc::new(Value::Env(env.clone()));
        let mut result = Value::make_inert();

        for expr in &exprs {
            result = eval(expr.clone(), env_val.clone())?;
        }

        result.as_val()
    }

    // not: boolean negation
    fn not(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        match &params[..] {
            [val] => {
                if let Value::Bool(b) = val.deref() {
                    Value::boolean(!matches!(b, crate::values::bools::Bool::True)).as_val()
                } else {
                    RuntimeError::type_error("not requires a boolean")
                }
            }
            _ => RuntimeError::type_error("not requires exactly 1 argument"),
        }
    }

    // $and?: operative that short-circuits on #f
    fn and(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let exprs = vals.operands()?;
        let env_val = Rc::new(Value::Env(env));

        for expr in &exprs {
            let result = eval(expr.clone(), env_val.clone())?;
            match result.deref() {
                Value::Bool(b) if !matches!(b, crate::values::bools::Bool::True) => return Value::boolean(false).as_val(),
                Value::Bool(_) => continue,
                _ => return RuntimeError::type_error("$and? requires boolean operands"),
            }
        }
        Value::boolean(true).as_val()
    }

    // $or?: operative that short-circuits on #t
    fn or(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let exprs = vals.operands()?;
        let env_val = Rc::new(Value::Env(env));

        for expr in &exprs {
            let result = eval(expr.clone(), env_val.clone())?;
            match result.deref() {
                Value::Bool(b) if matches!(b, crate::values::bools::Bool::True) => return Value::boolean(true).as_val(),
                Value::Bool(_) => continue,
                _ => return RuntimeError::type_error("$or? requires boolean operands"),
            }
        }
        Value::boolean(false).as_val()
    }

    // and?: applicative version (all args already evaluated)
    fn and_applicative(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;

        for arg in &args {
            match arg.deref() {
                Value::Bool(b) if !matches!(b, crate::values::bools::Bool::True) => return Value::boolean(false).as_val(),
                Value::Bool(_) => continue,
                _ => return RuntimeError::type_error("and? requires boolean arguments"),
            }
        }
        Value::boolean(true).as_val()
    }

    // or?: applicative version (all args already evaluated)
    fn or_applicative(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;

        for arg in &args {
            match arg.deref() {
                Value::Bool(b) if matches!(b, crate::values::bools::Bool::True) => return Value::boolean(true).as_val(),
                Value::Bool(_) => continue,
                _ => return RuntimeError::type_error("or? requires boolean arguments"),
            }
        }
        Value::boolean(false).as_val()
    }

    // $let operative: parallel bindings in new environment
    // ($let ((var1 val1) (var2 val2) ...) body ...)
    fn let_(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        if params.is_empty() {
            return RuntimeError::type_error("$let requires bindings and body");
        }

        let bindings_val = params[0].clone();
        let env_val = Rc::new(Value::Env(env.clone()));

        // Create new environment with current as parent
        let local_env = Env::new(vec![env]);

        // Evaluate all values in the ORIGINAL environment first (parallel binding)
        let bindings = bindings_val.operands()?;
        let mut evaluated_bindings: Vec<(Rc<Value>, Rc<Value>)> = Vec::new();
        for binding in &bindings {
            let var = binding.car()?;
            let val_expr = binding.cdr()?.car()?;
            let val = eval(val_expr.into(), env_val.clone())?;
            evaluated_bindings.push((var.into(), val));
        }

        // Now bind all variables in the local environment
        for (var, val) in evaluated_bindings {
            match_formals(&var, &val, &local_env)?;
        }

        // Evaluate body in local environment
        let local_env_val = Rc::new(Value::Env(local_env));
        let mut result = Value::make_inert();
        for i in 1..params.len() {
            result = eval(params[i].clone(), local_env_val.clone())?;
        }
        result.as_val()
    }

    // $let* operative: sequential bindings
    // ($let* ((var1 val1) (var2 val2) ...) body ...)
    fn let_star(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        if params.is_empty() {
            return RuntimeError::type_error("$let* requires bindings and body");
        }

        let bindings_val = params[0].clone();

        // Start with current environment
        let mut current_env = env;

        // Process each binding sequentially
        let bindings = bindings_val.operands()?;
        for binding in &bindings {
            let var = binding.car()?;
            let val_expr = binding.cdr()?.car()?;

            // Evaluate in current environment
            let current_env_val = Rc::new(Value::Env(current_env.clone()));
            let val = eval(val_expr.into(), current_env_val)?;

            // Create new environment and bind
            let new_env = Env::new(vec![current_env]);
            match_formals(&var.into(), &val, &new_env)?;
            current_env = new_env;
        }

        // Evaluate body in final environment
        let final_env_val = Rc::new(Value::Env(current_env));
        let mut result = Value::make_inert();
        for i in 1..params.len() {
            result = eval(params[i].clone(), final_env_val.clone())?;
        }
        result.as_val()
    }

    // $letrec operative: recursive bindings
    // ($letrec ((var1 val1) (var2 val2) ...) body ...)
    fn letrec(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        if params.is_empty() {
            return RuntimeError::type_error("$letrec requires bindings and body");
        }

        let bindings_val = params[0].clone();

        // Create local environment with current as parent
        let local_env = Env::new(vec![env]);
        let local_env_val = Rc::new(Value::Env(local_env.clone()));

        // First, bind all variables to #inert (placeholder)
        let bindings = bindings_val.operands()?;
        for binding in &bindings {
            let var = binding.car()?;
            if let Value::Symbol(sym) = var.deref() {
                local_env.borrow_mut().bind(sym.clone(), Value::make_inert());
            }
        }

        // Then evaluate all values in the local environment and rebind
        for binding in &bindings {
            let var = binding.car()?;
            let val_expr = binding.cdr()?.car()?;
            let val = eval(val_expr.into(), local_env_val.clone())?;
            match_formals(&var.into(), &val, &local_env)?;
        }

        // Evaluate body in local environment
        let mut result = Value::make_inert();
        for i in 1..params.len() {
            result = eval(params[i].clone(), local_env_val.clone())?;
        }
        result.as_val()
    }

    // $cond operative: evaluate clauses in order
    // ($cond (<test1> <body1> ...) (<test2> <body2> ...) ...)
    fn cond(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let clauses = vals.operands()?;
        let env_val = Rc::new(Value::Env(env.clone()));

        for clause in &clauses {
            // Each clause should be a list (test body...)
            if !matches!(clause.deref(), Value::Pair(_)) {
                return RuntimeError::type_error("$cond clause must be a list");
            }
            let test_expr = clause.car()?;
            let bodies = clause.cdr()?;

            // Evaluate the test
            let test_result = eval(test_expr.into(), env_val.clone())?;
            match test_result.deref() {
                Value::Bool(b) if matches!(b, crate::values::bools::Bool::True) => {
                    // Test passed, evaluate body expressions in order
                    let body_exprs: Rc<Value> = bodies.into();
                    let body_list = body_exprs.operands()?;
                    if body_list.is_empty() {
                        return Value::make_inert().as_val();
                    }
                    let mut result = Value::make_inert();
                    for body in &body_list {
                        result = eval(body.clone(), env_val.clone())?;
                    }
                    return result.as_val();
                }
                Value::Bool(_) => continue, // Test was #f, try next clause
                _ => return RuntimeError::type_error("$cond test must evaluate to a boolean"),
            }
        }

        // No clause matched
        Value::make_inert().as_val()
    }

    // apply: call an applicative with given argument list and optional environment
    // (apply applicative args) or (apply applicative args env)
    fn apply(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        match &params[..] {
            [applicative, args] => {
                // Use current environment
                if let Value::Combiner(c) = applicative.deref() {
                    if c.c_type != CombinerType::Applicative {
                        return RuntimeError::type_error("apply requires an applicative");
                    }
                    // Call applicative with the given argument list
                    let env_val = Rc::new(Value::Env(env.clone()));
                    call_applicative(c, args.clone(), env, env_val)
                } else {
                    RuntimeError::type_error("apply requires an applicative")
                }
            }
            [applicative, args, env_arg] => {
                // Use specified environment
                if let Value::Combiner(c) = applicative.deref() {
                    if c.c_type != CombinerType::Applicative {
                        return RuntimeError::type_error("apply requires an applicative");
                    }
                    if let Value::Env(e) = env_arg.deref() {
                        call_applicative(c, args.clone(), e.clone(), env_arg.clone())
                    } else {
                        RuntimeError::type_error("apply environment must be an environment")
                    }
                } else {
                    RuntimeError::type_error("apply requires an applicative")
                }
            }
            _ => RuntimeError::type_error("apply requires 2 or 3 arguments"),
        }
    }
}

/// Validate that a formal parameter tree is valid and collect all symbols
fn validate_formals(formals: &Rc<Value>, seen: &mut HashSet<String>) -> Result<(), RuntimeError> {
    match formals.deref() {
        Value::Symbol(Symbol(name)) => {
            if seen.contains(name) {
                return RuntimeError::type_error(format!("duplicate symbol in formals: {}", name));
            }
            seen.insert(name.clone());
            Ok(())
        }
        Value::Constant(crate::values::Constant::Ignore) => Ok(()),
        Value::Constant(crate::values::Constant::Null) => Ok(()),
        Value::Pair(_) => {
            let car = formals.car()?;
            let cdr = formals.cdr()?;
            validate_formals(&car, seen)?;
            validate_formals(&cdr, seen)?;
            Ok(())
        }
        _ => RuntimeError::type_error("invalid formal parameter tree"),
    }
}

impl Value {
    // helpers
    pub fn new_applicative(name: impl Into<String>, func: Func, _expr: Rc<Value>) -> Rc<Value> {
        Combiner::new_primitive(name, func, CombinerType::Applicative)
    }

    pub fn new_operative(name: impl Into<String>, func: Func, _expr: Rc<Value>) -> Rc<Value> {
        Combiner::new_primitive(name, func, CombinerType::Operative)
    }

    fn arguments(&self, env: Rc<Value>) -> Result<Vec<Rc<Value>>, RuntimeError> {
        let mut params = self.iter()
            .map(|v| eval(v, env.clone()))
            .collect::<Result<Vec<Rc<Value>>, RuntimeError>>()?;
        params.pop();
        Ok(params)
    }

    fn operands(&self) -> Result<Vec<Rc<Value>>, RuntimeError> {
        let mut params: Vec<Rc<Value>> = self.iter().collect();
        params.pop();
        Ok(params)
    }

    pub fn call(fun: Rc<Value>, env: Rc<Value>, params: Rc<Value>) -> CallResult {
        if let Value::Env(e) = env.deref() {
            match fun.deref() {
                Value::Combiner(c) => {
                    match c.c_type {
                        CombinerType::Operative => {
                            call_operative(c, params, e.clone(), env.clone())
                        }
                        CombinerType::Applicative => {
                            // Evaluate arguments first
                            let args = params.arguments(env.clone())?;
                            let args_list = Value::to_list(args)?;
                            call_applicative(c, args_list, e.clone(), env.clone())
                        }
                    }
                }
                _ => Err(RuntimeError::new(ErrorTypes::TypeError, "eval tried to call a non combiner")),
            }
        } else {
            RuntimeError::type_error("eval got a non environment")
        }
    }

    // primitives
    pub fn is_operative(val: Rc<Value>) -> ValueResult {
        is_val(val, &|val| matches!(val.deref(), Value::Combiner(Combiner{ c_type: CombinerType::Operative, .. })))
    }

    pub fn is_applicative(val: Rc<Value>) -> ValueResult {
        is_val(val, &|val| matches!(val.deref(), Value::Combiner(Combiner{ c_type: CombinerType::Applicative, .. })))
    }

    pub fn is_combiner(val: Rc<Value>) -> ValueResult {
        is_val(val, &|val| matches!(val.deref(), Value::Combiner(_)))
    }

    pub fn as_tail_call(&self) -> CallResult {
        Ok(CallResultType::Call(self.into()))
    }

    pub fn as_val(&self) -> CallResult {
        Ok(CallResultType::Value(self.into()))
    }
}

/// Call an operative combiner with unevaluated operands
fn call_operative(c: &Combiner, operands: Rc<Value>, env: EnvRef, env_val: Rc<Value>) -> CallResult {
    match &c.kind {
        CombinerKind::Primitive { func } => {
            func(operands, env)
        }
        CombinerKind::Compound { static_env, formals, eformal, body } => {
            // Create local environment with static_env as parent
            let local_env = Env::new(vec![static_env.clone()]);

            // Match formals to operands in local environment
            match_formals(&formals, &operands, &local_env)?;

            // Bind eformal to dynamic environment if it's a symbol
            if let Value::Symbol(sym) = eformal.deref() {
                local_env.borrow_mut().bind(sym.clone(), env_val);
            }

            // Evaluate body in local environment as tail context
            body.eval(Rc::new(Value::Env(local_env)))
        }
        CombinerKind::Wrapped { underlying } => {
            // Wrapped operatives shouldn't exist, but handle it
            if let Value::Combiner(inner) = underlying.deref() {
                call_operative(inner, operands, env, env_val)
            } else {
                RuntimeError::type_error("wrapped combiner is invalid")
            }
        }
    }
}

/// Call an applicative combiner with already-evaluated arguments
fn call_applicative(c: &Combiner, args: Rc<Value>, env: EnvRef, env_val: Rc<Value>) -> CallResult {
    match &c.kind {
        CombinerKind::Primitive { func } => {
            func(args, env)
        }
        CombinerKind::Compound { .. } => {
            // This shouldn't happen - compound combiners are always operatives
            // But if we get here, treat it like an operative call
            RuntimeError::type_error("compound combiner cannot be applicative directly")
        }
        CombinerKind::Wrapped { underlying } => {
            // Call the underlying combiner as an operative with the evaluated args
            if let Value::Combiner(inner) = underlying.deref() {
                call_operative(inner, args, env, env_val)
            } else {
                RuntimeError::type_error("wrapped combiner is invalid")
            }
        }
    }
}

/// Match a formal parameter tree against an operand tree, binding symbols in the environment
fn match_formals(formals: &Rc<Value>, operands: &Rc<Value>, env: &EnvRef) -> Result<(), RuntimeError> {
    match (formals.deref(), operands.deref()) {
        // Symbol binds to the entire operand
        (Value::Symbol(sym), _) => {
            env.borrow_mut().bind(sym.clone(), operands.clone());
            Ok(())
        }
        // #ignore matches anything
        (Value::Constant(crate::values::Constant::Ignore), _) => Ok(()),
        // () matches only ()
        (Value::Constant(crate::values::Constant::Null), Value::Constant(crate::values::Constant::Null)) => Ok(()),
        (Value::Constant(crate::values::Constant::Null), _) => {
            RuntimeError::type_error("formal/operand mismatch: expected ()")
        }
        // Pair matches pair recursively
        (Value::Pair(_), Value::Pair(_)) => {
            let formal_car = formals.car()?;
            let formal_cdr = formals.cdr()?;
            let operand_car = operands.car()?;
            let operand_cdr = operands.cdr()?;
            match_formals(&formal_car, &operand_car, env)?;
            match_formals(&formal_cdr, &operand_cdr, env)?;
            Ok(())
        }
        (Value::Pair(_), _) => {
            RuntimeError::type_error("formal/operand mismatch: expected pair")
        }
        _ => RuntimeError::type_error("invalid formal parameter tree"),
    }
}


#[cfg(test)]
mod tests {
    use yare::parameterized;

    use std::rc::Rc;
    use std::ops::Deref;
    use crate::values::{ Constant, Value, tests::sample_values };
    use crate::values::eval::eval;
    use crate::values::Combiner;

    use super::{CombinerType, CombinerKind};

    fn make_test_combiner(name: &str, c_type: CombinerType) -> Combiner {
        Combiner {
            name: name.to_string(),
            c_type,
            kind: CombinerKind::Primitive {
                func: &|_e, _en| Value::Constant(Constant::Null).as_val(),
            },
        }
    }

    #[test]
    fn test_is_combiner() {
        for val in sample_values() {
            let listified = val.as_pair();
            let is_applicative = Value::is_applicative(listified.clone()).expect("ok").is_true();
            let is_operative = Value::is_operative(listified.clone()).expect("ok").is_true();
            match val.deref() {
                Value::Combiner(Combiner{ c_type: CombinerType::Applicative, ..}) => assert!(is_applicative && !is_operative),
                Value::Combiner(Combiner{ c_type: CombinerType::Operative, ..}) => assert!(is_operative && !is_applicative),
                _ => {
                    assert!(!is_operative);
                    assert!(!is_applicative);
                },
            }
        }
    }

    #[test]
    fn test_is_combiner_multi() {
        for val in sample_values() {
            let listified = Value::cons(
                val.clone(),
                Value::cons(
                    val.clone(),
                    Value::cons(
                        val.clone(),
                        val.as_pair()
                    ).unwrap().into(),
                ).unwrap().into(),
            ).unwrap();
            let is_applicative = Value::is_applicative(listified.clone().into()).expect("ok").is_true();
            let is_operative = Value::is_operative(listified.into()).expect("ok").is_true();
            match val.deref() {
                Value::Combiner(Combiner{ c_type: CombinerType::Applicative, ..}) => assert!(is_applicative && !is_operative),
                Value::Combiner(Combiner{ c_type: CombinerType::Operative, ..}) => assert!(is_operative && !is_applicative),
                _ => {
                    assert!(!is_operative);
                    assert!(!is_applicative);
                },
            }
        }
    }

    #[parameterized(
        applicative = { make_test_combiner("applicative", CombinerType::Applicative) },
        operative = { make_test_combiner("operative", CombinerType::Operative) },
    )]
    fn test_is_eq_self(val: Combiner) {
        assert!(val.is_eq(&val).unwrap());
    }

    #[parameterized(
        applicatives = {
            make_test_combiner("applicative", CombinerType::Applicative),
            make_test_combiner("applicative", CombinerType::Applicative)
        },
        operatives = {
            make_test_combiner("operative", CombinerType::Operative),
            make_test_combiner("operative", CombinerType::Operative)
        },
    )]
    fn test_is_eq(val1: Combiner, val2: Combiner) {
        assert!(val1.is_eq(&val2).unwrap());
        assert!(val2.is_eq(&val1).unwrap());
    }

    #[parameterized(
        applicative_different_names = {
            make_test_combiner("applicativebla", CombinerType::Applicative),
            make_test_combiner("applicative", CombinerType::Applicative)
        },
        operatives_diff_names = {
            make_test_combiner("operativebla", CombinerType::Operative),
            make_test_combiner("operative", CombinerType::Operative)
        },
        different_types = {
            make_test_combiner("bla", CombinerType::Applicative),
            make_test_combiner("bla", CombinerType::Operative)
        },
    )]
    fn test_is_eq_not(val1: Combiner, val2: Combiner) {
        assert!(!val1.is_eq(&val2).unwrap());
        assert!(!val2.is_eq(&val1).unwrap());
    }

    #[parameterized(
        yes = { Value::to_list(
            vec![Value::boolean(true), Value::make_null(), Value::make_ignore()]).unwrap().into(), "()"
        },
        no = { Value::to_list(vec![
            Value::boolean(false), Value::make_null(), Value::make_ignore(),
        ]).unwrap().into(), "#ignore" },
    )]
    fn test_if(vals: Rc<Value>, expected: &str) {
        let env = Value::ground_env();
        if let Value::Env(env_obj) = env.deref() {
            let func = eval(Combiner::if_(vals, env_obj.clone()).unwrap().into(), env).expect("ok");
            assert_eq!(func.to_string(), format!("{expected}"));
        }
    }
}

use std::{fmt, ops::Deref};
use std::rc::Rc;
use std::collections::HashSet;
use crate::errors::{ RuntimeError, ErrorTypes };
use crate::values::{ Value, ValueResult, CallResult, CallResultType, Symbol, is_val };
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
            CombinerKind::Primitive { .. } => write!(f, "{}", self.name),
            CombinerKind::Compound { formals, body, .. } => {
                write!(f, "#[compound {} {}]", formals, body)
            }
            CombinerKind::Wrapped { underlying } => write!(f, "{}", underlying),
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
                let test_result = eval(test.clone(), env_val.clone())?;
                // $if requires a boolean test value
                match test_result.deref() {
                    Value::Bool(b) => {
                        if matches!(b, crate::values::bools::Bool::True) {
                            branch1.eval(env_val)
                        } else {
                            branch2.eval(env_val)
                        }
                    }
                    _ => RuntimeError::type_error("$if test must be a boolean"),
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

    fn set(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let env_val = Rc::new(Value::Env(env.clone()));
        let params = vals.operands()?;
        match &params[..] {
            [val1, val2] => {
                env_val.assign(val1.clone(), val2.clone())?.as_val()
            },
            _ => RuntimeError::type_error(format!("$set! requires 2 arguments")),
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
        bind(&env, "quote", CombinerType::Operative, &|vals, _| vals.car()?.as_val());
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
        bind(&env, "$set!", CombinerType::Operative, &Combiner::set);

        // Boolean operations
        bind(&env, "not", CombinerType::Applicative, &Combiner::not);
        bind(&env, "not?", CombinerType::Applicative, &Combiner::not);  // Kernel-style alias
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
        bind(&env, "integer?", CombinerType::Applicative, &|vals, _| Value::is_integer(vals)?.as_val());
        bind(&env, "pair?", CombinerType::Applicative, &|vals, _| Value::is_pair(vals)?.as_val());
        bind(&env, "list?", CombinerType::Applicative, &|vals, _| Value::is_list(vals)?.as_val());
        bind(&env, "string?", CombinerType::Applicative, &|vals, _| Value::is_string(vals)?.as_val());
        bind(&env, "symbol?", CombinerType::Applicative, &|vals, _| Value::is_symbol(vals)?.as_val());

        // Library - arithmetic
        bind(&env, "+", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::add, "+"));
        bind(&env, "-", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::minus, "-"));
        bind(&env, "*", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::multiply, "*"));
        bind(&env, "/", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::divide, "/"));
        bind(&env, "mod", CombinerType::Applicative, &|vals, _| number_applicative(vals, &Value::modulo, "mod"));

        // Comparisons (using Kernel-style names with ?)
        // Chained comparisons (work on 2+ arguments)
        bind(&env, "<?", CombinerType::Applicative, &Combiner::chained_less_than);
        bind(&env, "<=?", CombinerType::Applicative, &Combiner::chained_less_equal);
        bind(&env, ">?", CombinerType::Applicative, &Combiner::chained_greater_than);
        bind(&env, ">=?", CombinerType::Applicative, &Combiner::chained_greater_equal);
        bind(&env, "=?", CombinerType::Applicative, &Combiner::chained_numeric_equal);

        // Numeric predicates
        bind(&env, "zero?", CombinerType::Applicative, &Combiner::zero_pred);
        bind(&env, "positive?", CombinerType::Applicative, &Combiner::positive_pred);
        bind(&env, "negative?", CombinerType::Applicative, &Combiner::negative_pred);
        bind(&env, "odd?", CombinerType::Applicative, &Combiner::odd_pred);
        bind(&env, "even?", CombinerType::Applicative, &Combiner::even_pred);

        // Numeric functions
        bind(&env, "abs", CombinerType::Applicative, &Combiner::abs);
        bind(&env, "min", CombinerType::Applicative, &Combiner::min);
        bind(&env, "max", CombinerType::Applicative, &Combiner::max);

        // Library - list operations
        bind(&env, "car", CombinerType::Applicative, &|vals, _| vals.car()?.car()?.as_val());
        bind(&env, "cdr", CombinerType::Applicative, &|vals, _| vals.car()?.cdr()?.as_val());
        bind(&env, "list", CombinerType::Applicative, &|vals, _| vals.as_val());
        bind(&env, "list*", CombinerType::Applicative, &Combiner::list_star);
        bind(&env, "append", CombinerType::Applicative, &Combiner::append);
        bind(&env, "length", CombinerType::Applicative, &Combiner::length);
        bind(&env, "list-ref", CombinerType::Applicative, &Combiner::list_ref);
        bind(&env, "list-tail", CombinerType::Applicative, &Combiner::list_tail);
        bind(&env, "reverse", CombinerType::Applicative, &Combiner::reverse);
        bind(&env, "member", CombinerType::Applicative, &Combiner::member);
        bind(&env, "assoc", CombinerType::Applicative, &Combiner::assoc);
        bind(&env, "map", CombinerType::Applicative, &Combiner::map);
        bind(&env, "for-each", CombinerType::Applicative, &Combiner::for_each);
        bind(&env, "filter", CombinerType::Applicative, &Combiner::filter);
        bind(&env, "reduce", CombinerType::Applicative, &Combiner::reduce);

        // Environment operations
        bind(&env, "get-current-environment", CombinerType::Operative, &Combiner::get_current_env);
        bind(&env, "$binds?", CombinerType::Operative, &Combiner::binds);

        // Convenience accessors
        bind(&env, "cadr", CombinerType::Applicative, &|vals, _| vals.car()?.cdr()?.car()?.as_val());
        bind(&env, "caddr", CombinerType::Applicative, &|vals, _| vals.car()?.cdr()?.cdr()?.car()?.as_val());
        bind(&env, "caar", CombinerType::Applicative, &|vals, _| vals.car()?.car()?.car()?.as_val());
        bind(&env, "cdar", CombinerType::Applicative, &|vals, _| vals.car()?.car()?.cdr()?.as_val());
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
                    // Return the Rc directly to preserve identity for eq?
                    Ok(CallResultType::Value(c.unwrap()?))
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

        // Get body - wrap multiple body expressions in $sequence
        let body = if params.len() == 1 {
            Value::make_inert()
        } else if params.len() == 2 {
            params[1].clone()
        } else {
            // Multiple body expressions - wrap in $sequence
            let mut seq_args = Value::make_null();
            for i in (1..params.len()).rev() {
                seq_args = Value::cons(params[i].clone(), seq_args)?;
            }
            Value::cons(Value::make_symbol("$sequence"), seq_args)?
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

    // list*: construct list with last element as tail
    // (list* a b c d) = (a b c . d)
    fn list_star(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        if args.is_empty() {
            return RuntimeError::type_error("list* requires at least one argument");
        }
        if args.len() == 1 {
            return args[0].clone().as_val();
        }
        // Build list with last element as tail
        let mut result = args[args.len() - 1].clone();
        for i in (0..args.len() - 1).rev() {
            result = Value::cons(args[i].clone(), result)?;
        }
        result.as_val()
    }

    // append: concatenate lists
    fn append(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        if args.is_empty() {
            return Value::make_null().as_val();
        }
        if args.len() == 1 {
            return args[0].clone().as_val();
        }

        // Collect all elements from all lists except the last
        let mut elements: Vec<Rc<Value>> = Vec::new();
        for i in 0..args.len() - 1 {
            let list = &args[i];
            let items = list.operands()?;
            for item in items {
                elements.push(item);
            }
        }

        // Build result with last arg as tail
        let last = args[args.len() - 1].clone();
        let mut result = last;
        for elem in elements.into_iter().rev() {
            result = Value::cons(elem, result)?;
        }
        result.as_val()
    }

    // length: return length of list
    fn length(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [list] => {
                let items = list.operands()?;
                Number::int(items.len() as i64).as_val()
            }
            _ => RuntimeError::type_error("length requires exactly 1 argument"),
        }
    }

    // list-ref: get element at index
    fn list_ref(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [list, index] => {
                if let Value::Number(Number::Int(i)) = index.deref() {
                    let items = list.operands()?;
                    if *i < 0 || (*i as usize) >= items.len() {
                        return RuntimeError::type_error("list-ref index out of bounds");
                    }
                    items[*i as usize].clone().as_val()
                } else {
                    RuntimeError::type_error("list-ref index must be an integer")
                }
            }
            _ => RuntimeError::type_error("list-ref requires 2 arguments"),
        }
    }

    // list-tail: return tail starting at index
    fn list_tail(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [list, index] => {
                if let Value::Number(Number::Int(i)) = index.deref() {
                    let mut current = list.clone();
                    for _ in 0..*i {
                        match current.deref() {
                            Value::Pair(_) => current = current.cdr()?.into(),
                            _ => return RuntimeError::type_error("list-tail index out of bounds"),
                        }
                    }
                    current.as_val()
                } else {
                    RuntimeError::type_error("list-tail index must be an integer")
                }
            }
            _ => RuntimeError::type_error("list-tail requires 2 arguments"),
        }
    }

    // reverse: reverse a list
    fn reverse(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [list] => {
                let items = list.operands()?;
                let mut result = Value::make_null();
                for item in items {
                    result = Value::cons(item, result)?;
                }
                result.as_val()
            }
            _ => RuntimeError::type_error("reverse requires exactly 1 argument"),
        }
    }

    // member: find element in list, return tail or #f
    fn member(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [obj, list] => {
                let mut current = list.clone();
                loop {
                    match current.deref() {
                        Value::Constant(crate::values::Constant::Null) => {
                            return Value::boolean(false).as_val();
                        }
                        Value::Pair(_) => {
                            let head = current.car()?;
                            if Value::is_equal(obj.clone(), head.into())?.is_true() {
                                return current.as_val();
                            }
                            current = current.cdr()?.into();
                        }
                        _ => return RuntimeError::type_error("member requires a list"),
                    }
                }
            }
            _ => RuntimeError::type_error("member requires 2 arguments"),
        }
    }

    // assoc: find association in alist
    fn assoc(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [key, alist] => {
                let items = alist.operands()?;
                for item in &items {
                    if let Value::Pair(_) = item.deref() {
                        let item_key = item.car()?;
                        if Value::is_equal(key.clone(), item_key.into())?.is_true() {
                            return item.clone().as_val();
                        }
                    }
                }
                Value::boolean(false).as_val()
            }
            _ => RuntimeError::type_error("assoc requires 2 arguments"),
        }
    }

    // map: apply function to each element (supports multiple lists)
    fn map(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        if args.len() < 2 {
            return RuntimeError::type_error("map requires at least 2 arguments");
        }

        let func = args[0].clone();
        let env_val = Rc::new(Value::Env(env.clone()));

        // func must be an applicative
        let combiner = if let Value::Combiner(c) = func.deref() {
            if c.c_type != CombinerType::Applicative {
                return RuntimeError::type_error("map requires an applicative");
            }
            c
        } else {
            return RuntimeError::type_error("map requires an applicative");
        };

        if args.len() == 2 {
            // Single list case
            let items = args[1].operands()?;
            let mut results: Vec<Rc<Value>> = Vec::new();
            for item in &items {
                // Call the function directly with the item as argument
                let arg_list = Value::cons(item.clone(), Value::make_null())?;
                let result = call_applicative_full(combiner, arg_list, env.clone(), env_val.clone())?;
                results.push(result);
            }
            Value::to_list(results)?.as_val()
        } else {
            // Multiple lists case - zip and apply
            let lists: Vec<Vec<Rc<Value>>> = args[1..]
                .iter()
                .map(|list| list.operands())
                .collect::<Result<Vec<_>, _>>()?;

            // Check all lists have same length
            let len = lists[0].len();
            for list in &lists {
                if list.len() != len {
                    return RuntimeError::type_error("map: all lists must have same length");
                }
            }

            let mut results: Vec<Rc<Value>> = Vec::new();
            for i in 0..len {
                // Build argument list from ith element of each list
                let mut call_args = Value::make_null();
                for list in lists.iter().rev() {
                    call_args = Value::cons(list[i].clone(), call_args)?;
                }
                let result = call_applicative_full(combiner, call_args, env.clone(), env_val.clone())?;
                results.push(result);
            }
            Value::to_list(results)?.as_val()
        }
    }

    // for-each: apply function for side effects (supports multiple lists)
    fn for_each(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        if args.len() < 2 {
            return RuntimeError::type_error("for-each requires at least 2 arguments");
        }

        let func = args[0].clone();
        let env_val = Rc::new(Value::Env(env.clone()));

        // func must be an applicative
        let combiner = if let Value::Combiner(c) = func.deref() {
            if c.c_type != CombinerType::Applicative {
                return RuntimeError::type_error("for-each requires an applicative");
            }
            c
        } else {
            return RuntimeError::type_error("for-each requires an applicative");
        };

        if args.len() == 2 {
            // Single list case
            let items = args[1].operands()?;
            for item in &items {
                let arg_list = Value::cons(item.clone(), Value::make_null())?;
                call_applicative_full(combiner, arg_list, env.clone(), env_val.clone())?;
            }
        } else {
            // Multiple lists case - zip and apply
            let lists: Vec<Vec<Rc<Value>>> = args[1..]
                .iter()
                .map(|list| list.operands())
                .collect::<Result<Vec<_>, _>>()?;

            // Check all lists have same length
            let len = lists[0].len();
            for list in &lists {
                if list.len() != len {
                    return RuntimeError::type_error("for-each: all lists must have same length");
                }
            }

            for i in 0..len {
                // Build argument list from ith element of each list
                let mut call_args = Value::make_null();
                for list in lists.iter().rev() {
                    call_args = Value::cons(list[i].clone(), call_args)?;
                }
                call_applicative_full(combiner, call_args, env.clone(), env_val.clone())?;
            }
        }

        Value::make_inert().as_val()
    }

    // filter: select elements satisfying predicate
    fn filter(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [pred, list] => {
                let items = list.operands()?;
                let env_val = Rc::new(Value::Env(env.clone()));

                // pred must be an applicative
                let combiner = if let Value::Combiner(c) = pred.deref() {
                    if c.c_type != CombinerType::Applicative {
                        return RuntimeError::type_error("filter requires an applicative predicate");
                    }
                    c
                } else {
                    return RuntimeError::type_error("filter requires an applicative predicate");
                };

                let mut results: Vec<Rc<Value>> = Vec::new();
                for item in &items {
                    let arg_list = Value::cons(item.clone(), Value::make_null())?;
                    let result = call_applicative_full(combiner, arg_list, env.clone(), env_val.clone())?;
                    if result.is_true() {
                        results.push(item.clone());
                    }
                }

                Value::to_list(results)?.as_val()
            }
            _ => RuntimeError::type_error("filter requires 2 arguments"),
        }
    }

    // reduce: fold left with binary function (reduce func identity list)
    fn reduce(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [func, identity, list] => {
                let items = list.operands()?;
                let env_val = Rc::new(Value::Env(env.clone()));

                // func must be an applicative
                let combiner = if let Value::Combiner(c) = func.deref() {
                    if c.c_type != CombinerType::Applicative {
                        return RuntimeError::type_error("reduce requires an applicative");
                    }
                    c
                } else {
                    return RuntimeError::type_error("reduce requires an applicative");
                };

                let mut acc = identity.clone();
                for item in &items {
                    // (func acc item)
                    let arg_list = Value::cons(acc, Value::cons(item.clone(), Value::make_null())?)?;
                    acc = call_applicative_full(combiner, arg_list, env.clone(), env_val.clone())?;
                }

                acc.as_val()
            }
            _ => RuntimeError::type_error("reduce requires 3 arguments: func, identity, list"),
        }
    }

    // get-current-environment: returns the dynamic environment
    fn get_current_env(_vals: Rc<Value>, env: EnvRef) -> CallResult {
        Rc::new(Value::Env(env)).as_val()
    }

    // $binds?: check if a symbol is bound in an environment
    // ($binds? env symbol)
    fn binds(vals: Rc<Value>, env: EnvRef) -> CallResult {
        let params = vals.operands()?;
        match &params[..] {
            [env_expr, symbol] => {
                let env_val = Rc::new(Value::Env(env.clone()));
                let target_env = eval(env_expr.clone(), env_val)?;
                if let Value::Env(e) = target_env.deref() {
                    // Check if symbol is bound - symbol is NOT evaluated
                    if let Value::Symbol(sym) = symbol.deref() {
                        let bound = e.borrow().get(sym.clone()).is_some();
                        Value::boolean(bound).as_val()
                    } else {
                        RuntimeError::type_error("$binds? requires a symbol")
                    }
                } else {
                    RuntimeError::type_error("$binds? requires an environment")
                }
            }
            _ => RuntimeError::type_error("$binds? requires 2 arguments"),
        }
    }

    // Chained comparisons
    fn chained_less_than(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        chained_comparison(vals, &|a, b| Number::less_than(a, b))
    }

    fn chained_less_equal(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        chained_comparison(vals, &|a, b| Number::less_than_or_equal(a, b))
    }

    fn chained_greater_than(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        chained_comparison(vals, &|a, b| Number::greater_than(a, b))
    }

    fn chained_greater_equal(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        chained_comparison(vals, &|a, b| Number::greater_than_or_equal(a, b))
    }

    fn chained_numeric_equal(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        chained_comparison(vals, &|a, b| Number::numeric_equal(a, b))
    }

    // Numeric predicates
    fn zero_pred(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [n] => {
                if let Value::Number(num) = n.deref() {
                    let is_zero = match num {
                        Number::Int(i) => *i == 0,
                        Number::Float(f) => *f == 0.0,
                    };
                    Value::boolean(is_zero).as_val()
                } else {
                    RuntimeError::type_error("zero? requires a number")
                }
            }
            _ => RuntimeError::type_error("zero? requires 1 argument"),
        }
    }

    fn positive_pred(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [n] => {
                if let Value::Number(num) = n.deref() {
                    let is_positive = match num {
                        Number::Int(i) => *i > 0,
                        Number::Float(f) => *f > 0.0,
                    };
                    Value::boolean(is_positive).as_val()
                } else {
                    RuntimeError::type_error("positive? requires a number")
                }
            }
            _ => RuntimeError::type_error("positive? requires 1 argument"),
        }
    }

    fn negative_pred(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [n] => {
                if let Value::Number(num) = n.deref() {
                    let is_negative = match num {
                        Number::Int(i) => *i < 0,
                        Number::Float(f) => *f < 0.0,
                    };
                    Value::boolean(is_negative).as_val()
                } else {
                    RuntimeError::type_error("negative? requires a number")
                }
            }
            _ => RuntimeError::type_error("negative? requires 1 argument"),
        }
    }

    fn odd_pred(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [n] => {
                if let Value::Number(Number::Int(i)) = n.deref() {
                    Value::boolean(*i % 2 != 0).as_val()
                } else {
                    RuntimeError::type_error("odd? requires an integer")
                }
            }
            _ => RuntimeError::type_error("odd? requires 1 argument"),
        }
    }

    fn even_pred(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [n] => {
                if let Value::Number(Number::Int(i)) = n.deref() {
                    Value::boolean(*i % 2 == 0).as_val()
                } else {
                    RuntimeError::type_error("even? requires an integer")
                }
            }
            _ => RuntimeError::type_error("even? requires 1 argument"),
        }
    }

    // abs: absolute value
    fn abs(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        match &args[..] {
            [n] => {
                if let Value::Number(num) = n.deref() {
                    let result = match num {
                        Number::Int(i) => Number::Int(i.abs()),
                        Number::Float(f) => Number::Float(f.abs()),
                    };
                    Rc::new(Value::Number(result)).as_val()
                } else {
                    RuntimeError::type_error("abs requires a number")
                }
            }
            _ => RuntimeError::type_error("abs requires 1 argument"),
        }
    }

    // min: find minimum value
    fn min(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        if args.is_empty() {
            return RuntimeError::type_error("min requires at least 1 argument");
        }
        let mut result = args[0].clone();
        for arg in args.iter().skip(1) {
            if Number::less_than(arg, &result)?.is_true() {
                result = arg.clone();
            }
        }
        result.as_val()
    }

    // max: find maximum value
    fn max(vals: Rc<Value>, _env: EnvRef) -> CallResult {
        let args = vals.operands()?;
        if args.is_empty() {
            return RuntimeError::type_error("max requires at least 1 argument");
        }
        let mut result = args[0].clone();
        for arg in args.iter().skip(1) {
            if Number::greater_than(arg, &result)?.is_true() {
                result = arg.clone();
            }
        }
        result.as_val()
    }
}

/// Chained comparison helper
fn chained_comparison(vals: Rc<Value>, compare: &dyn Fn(&Rc<Value>, &Rc<Value>) -> ValueResult) -> CallResult {
    let args = vals.operands()?;
    if args.len() < 2 {
        return RuntimeError::type_error("comparison requires at least 2 arguments");
    }

    for i in 0..args.len() - 1 {
        if !compare(&args[i], &args[i + 1])?.is_true() {
            return Value::boolean(false).as_val();
        }
    }
    Value::boolean(true).as_val()
}

/// Call an applicative and fully evaluate any tail calls (trampoline)
fn call_applicative_full(c: &Combiner, args: Rc<Value>, env: EnvRef, env_val: Rc<Value>) -> Result<Rc<Value>, RuntimeError> {
    let mut result = call_applicative(c, args, env.clone(), env_val.clone())?;
    loop {
        match result {
            CallResultType::Value(v) => return Ok(v),
            CallResultType::Call(c) => {
                result = c.eval(env_val.clone())?;
            }
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

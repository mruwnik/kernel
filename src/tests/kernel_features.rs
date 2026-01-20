/// Comprehensive tests for Kernel language features
/// Based on the Revised-1 Report on the Kernel Programming Language
///
/// These tests cover: $vau, wrap, unwrap, $lambda, $sequence, list*, apply, $cond,
/// arithmetic operations, comparison predicates, and more.

#[cfg(test)]
mod kernel_tests {
    use std::rc::Rc;
    use crate::values::Value;
    use crate::values::eval::eval;
    use crate::lexemes;
    use crate::tokens;

    /// Helper to parse and evaluate a Kernel expression
    fn eval_expr(code: &str) -> Result<Rc<Value>, String> {
        let mut chars = code.chars();
        let lexes = lexemes::get_lexemes(&mut chars).map_err(|e| e.to_string())?;
        let values = tokens::parse(lexes).map_err(|e| e.to_string())?;
        let env = Value::ground_env();

        values.into_iter()
            .map(|v| eval(v, env.clone()).map_err(|e| e.to_string()))
            .last()
            .unwrap_or_else(|| Ok(Value::make_inert()))
    }

    fn eval_expr_in_env(code: &str, env: Rc<Value>) -> Result<Rc<Value>, String> {
        let mut chars = code.chars();
        let lexes = lexemes::get_lexemes(&mut chars).map_err(|e| e.to_string())?;
        let values = tokens::parse(lexes).map_err(|e| e.to_string())?;

        values.into_iter()
            .map(|v| eval(v, env.clone()).map_err(|e| e.to_string()))
            .last()
            .unwrap_or_else(|| Ok(Value::make_inert()))
    }

    /// Helper to assert expression evaluates to expected string representation
    fn assert_eval(code: &str, expected: &str) {
        match eval_expr(code) {
            Ok(result) => assert_eq!(result.to_string(), expected, "Code: {}", code),
            Err(e) => panic!("Failed to evaluate '{}': {}", code, e),
        }
    }

    /// Helper to assert expression causes an error
    fn assert_eval_error(code: &str, error_substring: &str) {
        match eval_expr(code) {
            Ok(result) => panic!("Expected error containing '{}' but got: {} (code: {})",
                                 error_substring, result, code),
            Err(e) => assert!(e.contains(error_substring),
                             "Expected error containing '{}' but got: {} (code: {})",
                             error_substring, e, code),
        }
    }

    // ==================== $vau tests ====================
    // $vau is the fundamental operative constructor
    // ($vau <formals> <eformal> <body>)
    // - <formals>: formal parameter tree matched against operand tree
    // - <eformal>: symbol bound to dynamic environment, or #ignore
    // - <body>: expression(s) to evaluate in the local environment

    mod vau_basic {
        use super::*;

        #[test]
        fn vau_returns_operative() {
            // $vau should return an operative
            assert_eval("(operative? ($vau () #ignore #inert))", "#t");
        }

        #[test]
        fn vau_not_applicative() {
            // Result of $vau is not an applicative
            assert_eval("(applicative? ($vau () #ignore #inert))", "#f");
        }

        #[test]
        fn vau_identity_operative() {
            // Simple operative that returns its operand unevaluated
            assert_eval("(($vau (x) #ignore x) (+ 1 2))", "(+ 1 2)");
        }

        #[test]
        fn vau_returns_body_result() {
            // Body expression result is returned
            assert_eval("(($vau () #ignore 42) )", "42");
        }

        #[test]
        fn vau_returns_inert_constant() {
            assert_eval("(($vau () #ignore #inert))", "#inert");
        }

        #[test]
        fn vau_empty_operands() {
            // Operative with no formal parameters
            assert_eval("(($vau () #ignore (+ 1 2)))", "3");
        }

        #[test]
        fn vau_single_operand() {
            // Bind single operand to symbol
            assert_eval("(($vau (x) #ignore x) 42)", "42");
        }

        #[test]
        fn vau_operand_not_evaluated() {
            // Operands are NOT evaluated before binding
            assert_eval("(($vau (x) #ignore x) (+ 1 2))", "(+ 1 2)");
        }

        #[test]
        fn vau_multiple_operands() {
            // Multiple operands bound to formals
            assert_eval("(($vau (a b c) #ignore b) 1 2 3)", "2");
        }

        #[test]
        fn vau_rest_operands() {
            // Dotted formal captures rest as list
            assert_eval("(($vau (a . rest) #ignore rest) 1 2 3 4)", "(2 3 4)");
        }

        #[test]
        fn vau_all_rest() {
            // Single symbol captures all operands as list
            assert_eval("(($vau args #ignore args) 1 2 3)", "(1 2 3)");
        }

        #[test]
        fn vau_ignore_operands() {
            // #ignore in formals ignores that position
            assert_eval("(($vau (#ignore b) #ignore b) 1 2)", "2");
        }

        #[test]
        fn vau_null_matches_null() {
            // () in formals requires () operand
            assert_eval("(($vau (()) #ignore #t) ())", "#t");
        }
    }

    mod vau_environment {
        use super::*;

        #[test]
        fn vau_eformal_captures_dynamic_env() {
            // eformal symbol is bound to the dynamic environment
            assert_eval("
                ($define! x 42)
                (($vau () e (eval (quote x) e)))
            ", "42");
        }

        #[test]
        fn vau_eval_in_dynamic_env() {
            // Can evaluate operands in dynamic environment
            assert_eval("
                ($define! y 10)
                (($vau (expr) e (eval expr e)) (+ y 5))
            ", "15");
        }

        #[test]
        fn vau_ignore_dynamic_env() {
            // #ignore as eformal doesn't bind dynamic env
            assert_eval("(($vau (x) #ignore x) 42)", "42");
        }

        #[test]
        fn vau_static_env_captured() {
            // The environment where $vau is evaluated becomes static env
            assert_eval("
                ($define! make-adder
                    ($lambda (n)
                        ($vau (x) e (+ n (eval x e)))))
                ($define! add5 (make-adder 5))
                (add5 10)
            ", "15");
        }

        #[test]
        fn vau_static_env_lookup() {
            // Body can access bindings from static environment
            assert_eval("
                ($define! outer 100)
                ($define! f ($vau () #ignore outer))
                (f)
            ", "100");
        }

        #[test]
        fn vau_local_env_shadows_static() {
            // Formals shadow static environment bindings
            assert_eval("
                ($define! x 1)
                (($vau (x) #ignore x) 2)
            ", "2");
        }

        #[test]
        fn vau_local_env_parent_is_static() {
            // Local env has static env as parent
            assert_eval("
                ($define! x 1)
                ($define! y 2)
                (($vau (x) #ignore (+ x y)) 10)
            ", "12");
        }
    }

    mod vau_formal_parameter_trees {
        use super::*;

        #[test]
        fn vau_nested_formals_pair() {
            // Destructure a pair
            assert_eval("(($vau ((a . b)) #ignore a) (1 . 2))", "1");
            assert_eval("(($vau ((a . b)) #ignore b) (1 . 2))", "2");
        }

        #[test]
        fn vau_nested_formals_list() {
            // Destructure a list
            assert_eval("(($vau ((a b c)) #ignore b) (1 2 3))", "2");
        }

        #[test]
        fn vau_deeply_nested_formals() {
            // Deep destructuring - formals must match operand structure
            // Note: operand `((42))` gets wrapped in operand list, so formals need 3 levels
            assert_eval("(($vau (((a))) #ignore a) ((42)))", "42");
        }

        #[test]
        fn vau_mixed_nested_formals() {
            // Mix of nesting
            assert_eval("(($vau ((a b) c) #ignore (list a b c)) (1 2) 3)", "(1 2 3)");
        }

        #[test]
        fn vau_formals_with_ignore() {
            // #ignore at various positions in tree
            assert_eval("(($vau ((#ignore b)) #ignore b) (1 2))", "2");
            assert_eval("(($vau ((a #ignore c)) #ignore (list a c)) (1 2 3))", "(1 3)");
        }

        #[test]
        fn vau_formals_dotted_nested() {
            // Dotted pair in nested position
            assert_eval("(($vau ((a . b)) #ignore b) (1 2 3))", "(2 3)");
        }
    }

    mod vau_errors {
        use super::*;

        #[test]
        fn vau_too_few_operands() {
            // Not enough operands for formals
            assert_eval_error("(($vau (a b c) #ignore a) 1 2)", "match");
        }

        #[test]
        fn vau_too_many_operands() {
            // Too many operands
            assert_eval_error("(($vau (a) #ignore a) 1 2 3)", "match");
        }

        #[test]
        fn vau_null_mismatch() {
            // () in formals requires () in operands
            assert_eval_error("(($vau (()) #ignore #t) 1)", "match");
        }

        #[test]
        fn vau_pair_expected() {
            // Pair expected but got atom
            assert_eval_error("(($vau ((a . b)) #ignore a) 42)", "match");
        }

        #[test]
        fn vau_duplicate_symbol_error() {
            // Same symbol cannot appear twice in formals
            assert_eval_error("($vau (x x) #ignore x)", "duplicate");
        }

        #[test]
        fn vau_eformal_in_formals_error() {
            // eformal cannot also appear in formals
            assert_eval_error("($vau (e) e e)", "duplicate");
        }

        #[test]
        fn vau_invalid_formals() {
            // Non-symbol/pair/null/#ignore in formals
            assert_eval_error("($vau (42) #ignore #t)", "formal");
        }

        #[test]
        fn vau_invalid_eformal() {
            // eformal must be symbol or #ignore
            assert_eval_error("($vau () 42 #t)", "eformal");
        }
    }

    mod vau_immutability {
        use super::*;

        #[test]
        fn vau_body_is_immutable_copy() {
            // The body is taken literally from the $vau form (not evaluated)
            // So ($vau () #ignore (+ 1 2)) has body (+ 1 2)
            assert_eval("(($vau () #ignore (+ 1 2)) )", "3");
        }

        #[test]
        fn vau_formals_is_immutable_copy() {
            // The formals parameter tree is used literally (not evaluated)
            // So ($vau x #ignore x) binds x to all operands
            assert_eval("(($vau x #ignore x) 1 2 3)", "(1 2 3)");
        }
    }

    // ==================== wrap tests ====================
    // wrap takes a combiner and returns an applicative
    // The applicative evaluates its operands before passing to underlying combiner

    mod wrap_tests {
        use super::*;

        #[test]
        fn wrap_returns_applicative() {
            assert_eval("(applicative? (wrap ($vau () #ignore #t)))", "#t");
        }

        #[test]
        fn wrap_not_operative() {
            assert_eval("(operative? (wrap ($vau () #ignore #t)))", "#f");
        }

        #[test]
        fn wrap_evaluates_arguments() {
            // Wrapped operative receives evaluated arguments
            assert_eval("
                ((wrap ($vau (x) #ignore x)) (+ 1 2))
            ", "3");
        }

        #[test]
        fn wrap_multiple_args_evaluated() {
            assert_eval("
                ((wrap ($vau (a b) #ignore (cons a b))) (+ 1 1) (+ 2 2))
            ", "(2 . 4)");
        }

        #[test]
        fn wrap_preserves_combiner_behavior() {
            // Underlying combiner still works the same, just with evaluated args
            assert_eval("
                ($define! raw-if ($vau (test yes no) e
                    ($if (eval test e) (eval yes e) (eval no e))))
                ($define! wrapped-if (wrap raw-if))
                (wrapped-if #t 1 2)
            ", "1");
        }

        #[test]
        fn wrap_operative_twice() {
            // Can wrap an already wrapped combiner
            assert_eval("
                ($define! f ($vau (x) #ignore x))
                ($define! g (wrap (wrap f)))
                (applicative? g)
            ", "#t");
        }

        #[test]
        fn wrap_primitive_operative() {
            // Can wrap primitive operatives
            assert_eval("(applicative? (wrap $if))", "#t");
        }

        #[test]
        fn wrap_primitive_applicative() {
            // Can wrap primitive applicatives (double wrap)
            assert_eval("(applicative? (wrap cons))", "#t");
        }

        #[test]
        fn wrap_error_non_combiner() {
            // Error if argument is not a combiner
            assert_eval_error("(wrap 42)", "combiner");
            assert_eval_error("(wrap #t)", "combiner");
            assert_eval_error("(wrap ())", "combiner");
        }
    }

    // ==================== unwrap tests ====================
    // unwrap returns the underlying combiner of an applicative

    mod unwrap_tests {
        use super::*;

        #[test]
        fn unwrap_returns_underlying() {
            // unwrap returns the combiner that was wrapped
            assert_eval("
                ($define! f ($vau (x) #ignore x))
                ($define! g (wrap f))
                (eq? (unwrap g) f)
            ", "#t");
        }

        #[test]
        fn unwrap_wrapped_operative_is_operative() {
            assert_eval("(operative? (unwrap (wrap ($vau () #ignore #t))))", "#t");
        }

        #[test]
        fn unwrap_primitive_applicative() {
            // Unwrapping primitive applicative returns an operative
            assert_eval("(operative? (unwrap cons))", "#t");
        }

        #[test]
        fn unwrap_then_call() {
            // Can call the unwrapped combiner
            assert_eval("
                ((unwrap list) 1 2 3)
            ", "(1 2 3)");
        }

        #[test]
        fn unwrap_error_non_applicative() {
            // Error if not an applicative
            assert_eval_error("(unwrap ($vau () #ignore #t))", "applicative");
            assert_eval_error("(unwrap $if)", "applicative");
        }

        #[test]
        fn unwrap_error_non_combiner() {
            assert_eval_error("(unwrap 42)", "applicative");
            assert_eval_error("(unwrap ())", "applicative");
        }

        #[test]
        fn wrap_unwrap_identity_for_applicative() {
            // (wrap (unwrap applicative)) should behave same as applicative
            assert_eval("
                ($define! f (wrap (unwrap list)))
                (f 1 2 3)
            ", "(1 2 3)");
        }

        #[test]
        fn unwrap_double_wrapped() {
            // Unwrapping double-wrapped returns once-wrapped (still applicative)
            assert_eval("(applicative? (unwrap (wrap (wrap ($vau () #ignore #t)))))", "#t");
        }
    }

    // ==================== $lambda tests ====================
    // $lambda is equivalent to (wrap ($vau <formals> #ignore . <body>))

    mod lambda_tests {
        use super::*;

        #[test]
        fn lambda_returns_applicative() {
            assert_eval("(applicative? ($lambda () #t))", "#t");
        }

        #[test]
        fn lambda_not_operative() {
            assert_eval("(operative? ($lambda () #t))", "#f");
        }

        #[test]
        fn lambda_evaluates_arguments() {
            assert_eval("(($lambda (x) x) (+ 1 2))", "3");
        }

        #[test]
        fn lambda_no_args() {
            assert_eval("(($lambda () 42))", "42");
        }

        #[test]
        fn lambda_single_arg() {
            assert_eval("(($lambda (x) (+ x 1)) 5)", "6");
        }

        #[test]
        fn lambda_multiple_args() {
            assert_eval("(($lambda (a b c) (+ a (+ b c))) 1 2 3)", "6");
        }

        #[test]
        fn lambda_rest_args() {
            assert_eval("(($lambda args args) 1 2 3)", "(1 2 3)");
        }

        #[test]
        fn lambda_dotted_args() {
            assert_eval("(($lambda (a . rest) rest) 1 2 3)", "(2 3)");
        }

        #[test]
        fn lambda_destructuring() {
            assert_eval("(($lambda ((a b)) (+ a b)) (list 1 2))", "3");
        }

        #[test]
        fn lambda_closure() {
            // Lambda captures its static environment
            assert_eval("
                ($define! make-counter
                    ($lambda (start)
                        ($lambda () start)))
                ($define! counter (make-counter 42))
                (counter)
            ", "42");
        }

        #[test]
        fn lambda_nested() {
            assert_eval("
                ((($lambda (x) ($lambda (y) (+ x y))) 10) 5)
            ", "15");
        }

        #[test]
        fn lambda_recursive_via_define() {
            // Lambda can be recursive via $define!
            assert_eval("
                ($define! fact
                    ($lambda (n)
                        ($if (eq? n 0)
                            1
                            (* n (fact (- n 1))))))
                (fact 5)
            ", "120");
        }

        #[test]
        fn lambda_ignores_dynamic_env() {
            // $lambda does NOT capture dynamic environment
            // (this is the key difference from using $vau directly with eformal)
            assert_eval("
                ($define! f ($lambda () x))
                ($define! x 1)
                ($define! call-with-x
                    ($lambda (proc)
                        ($define! x 2)
                        (proc)))
                (call-with-x f)
            ", "1");
        }

        #[test]
        fn lambda_multiple_body_forms() {
            // Multiple body expressions (needs $sequence)
            assert_eval("
                ($define! x 0)
                (($lambda ()
                    ($define! x 1)
                    ($define! x (+ x 1))
                    x))
            ", "2");
        }
    }

    // ==================== $sequence tests ====================
    // $sequence evaluates expressions in order, returns last result

    mod sequence_tests {
        use super::*;

        #[test]
        fn sequence_empty() {
            // Empty sequence returns #inert
            assert_eval("($sequence)", "#inert");
        }

        #[test]
        fn sequence_single() {
            assert_eval("($sequence 42)", "42");
        }

        #[test]
        fn sequence_returns_last() {
            assert_eval("($sequence 1 2 3)", "3");
        }

        #[test]
        fn sequence_evaluates_all() {
            // All expressions are evaluated
            assert_eval("
                ($define! x 0)
                ($sequence
                    ($define! x (+ x 1))
                    ($define! x (+ x 1))
                    ($define! x (+ x 1))
                    x)
            ", "3");
        }

        #[test]
        fn sequence_left_to_right() {
            // Evaluation happens left to right
            assert_eval("
                ($define! log ())
                ($sequence
                    ($define! log (cons 1 log))
                    ($define! log (cons 2 log))
                    ($define! log (cons 3 log))
                    log)
            ", "(3 2 1)");
        }

        #[test]
        fn sequence_side_effects() {
            // Side effects from earlier expressions visible to later ones
            assert_eval("
                ($define! x 1)
                ($sequence
                    ($define! x (+ x 10))
                    x)
            ", "11");
        }

        #[test]
        fn sequence_nested() {
            assert_eval("
                ($sequence
                    ($sequence 1 2)
                    ($sequence 3 4))
            ", "4");
        }

        #[test]
        fn sequence_with_expressions() {
            assert_eval("
                ($sequence
                    (+ 1 2)
                    (* 3 4))
            ", "12");
        }
    }

    // ==================== list* tests ====================
    // list* is like list but last argument becomes cdr of last pair

    mod list_star_tests {
        use super::*;

        #[test]
        fn list_star_single() {
            // Single argument is returned as-is
            assert_eval("(list* 42)", "42");
            assert_eval("(list* ())", "()");
            assert_eval("(list* (list 1 2))", "(1 2)");
        }

        #[test]
        fn list_star_two_args() {
            // Two args makes a pair
            assert_eval("(list* 1 2)", "(1 . 2)");
        }

        #[test]
        fn list_star_builds_list() {
            // Multiple args with list as last
            assert_eval("(list* 1 2 3 ())", "(1 2 3)");
        }

        #[test]
        fn list_star_improper_list() {
            // Non-list last arg makes improper list
            assert_eval("(list* 1 2 3)", "(1 2 . 3)");
        }

        #[test]
        fn list_star_prepend_to_list() {
            assert_eval("(list* 1 2 (list 3 4))", "(1 2 3 4)");
        }

        #[test]
        fn list_star_evaluated() {
            // Arguments are evaluated (it's an applicative)
            assert_eval("(list* (+ 1 1) (+ 2 2))", "(2 . 4)");
        }

        #[test]
        fn list_star_many_args() {
            assert_eval("(list* 1 2 3 4 5 ())", "(1 2 3 4 5)");
        }

        #[test]
        fn list_star_single_with_list() {
            assert_eval("(list* (list 1 2 3))", "(1 2 3)");
        }
    }

    // ==================== apply tests ====================
    // apply calls an applicative with a given argument list

    mod apply_tests {
        use super::*;

        #[test]
        fn apply_basic() {
            assert_eval("(apply + (list 1 2 3))", "6");
        }

        #[test]
        fn apply_with_list() {
            assert_eval("(apply list (list 1 2 3))", "(1 2 3)");
        }

        #[test]
        fn apply_cons() {
            assert_eval("(apply cons (list 1 2))", "(1 . 2)");
        }

        #[test]
        fn apply_empty_args() {
            assert_eval("(apply + ())", "0");
        }

        #[test]
        fn apply_with_environment() {
            // apply passes through to the combiner - here we test that a lambda
            // defined in the current env works correctly with apply
            assert_eval("
                ($define! x 42)
                ($define! lookup-x ($lambda () x))
                (apply lookup-x ())
            ", "42");
        }

        #[test]
        fn apply_default_environment() {
            // Without env argument, uses empty environment
            assert_eval("(apply ($lambda () 42) ())", "42");
        }

        #[test]
        fn apply_lambda() {
            assert_eval("(apply ($lambda (a b) (+ a b)) (list 3 4))", "7");
        }

        #[test]
        fn apply_nested() {
            assert_eval("(apply apply (list + (list 1 2 3)))", "6");
        }

        #[test]
        fn apply_error_non_applicative() {
            // First arg must be applicative
            assert_eval_error("(apply $if (list #t 1 2))", "applicative");
            assert_eval_error("(apply 42 ())", "applicative");
        }

        #[test]
        fn apply_error_non_list() {
            // + requires numbers/list, so passing a non-list atom gives error
            assert_eval_error("(apply + 42)", "numbers");
        }

        #[test]
        fn apply_with_improper_argument_tree() {
            // apply can use any operand tree, not just lists
            assert_eval("
                (apply ($lambda x x) 42)
            ", "42");
        }
    }

    // ==================== $cond tests ====================
    // $cond is a multi-way conditional

    mod cond_tests {
        use super::*;

        #[test]
        fn cond_empty() {
            // No clauses returns #inert
            assert_eval("($cond)", "#inert");
        }

        #[test]
        fn cond_single_true() {
            assert_eval("($cond (#t 42))", "42");
        }

        #[test]
        fn cond_single_false() {
            assert_eval("($cond (#f 42))", "#inert");
        }

        #[test]
        fn cond_first_true() {
            assert_eval("($cond (#t 1) (#t 2) (#t 3))", "1");
        }

        #[test]
        fn cond_second_true() {
            assert_eval("($cond (#f 1) (#t 2) (#t 3))", "2");
        }

        #[test]
        fn cond_last_true() {
            assert_eval("($cond (#f 1) (#f 2) (#t 3))", "3");
        }

        #[test]
        fn cond_all_false() {
            assert_eval("($cond (#f 1) (#f 2) (#f 3))", "#inert");
        }

        #[test]
        fn cond_evaluates_test() {
            assert_eval("($cond ((eq? 1 1) 42))", "42");
            assert_eval("($cond ((eq? 1 2) 42))", "#inert");
        }

        #[test]
        fn cond_evaluates_consequent() {
            assert_eval("($cond (#t (+ 1 2)))", "3");
        }

        #[test]
        fn cond_short_circuits() {
            // Later clauses not evaluated if earlier one matches
            assert_eval("
                ($define! x 0)
                ($cond
                    (#t ($define! x 1))
                    (#t ($define! x 2)))
                x
            ", "1");
        }

        #[test]
        fn cond_tests_evaluated_in_order() {
            // Tests evaluated left to right until one is true
            assert_eval("
                ($define! log ())
                ($cond
                    (($sequence ($define! log (cons 1 log)) #f) #f)
                    (($sequence ($define! log (cons 2 log)) #t) log))
            ", "(2 1)");
        }

        #[test]
        fn cond_multiple_body_expressions() {
            // Each clause can have multiple body expressions
            assert_eval("
                ($define! x 0)
                ($cond
                    (#t
                        ($define! x (+ x 1))
                        ($define! x (+ x 1))
                        x))
            ", "2");
        }

        #[test]
        fn cond_error_non_boolean_test() {
            // Test must evaluate to boolean
            assert_eval_error("($cond (42 #t))", "boolean");
            assert_eval_error("($cond (() #t))", "boolean");
        }
    }

    // ==================== Integration tests ====================
    // Tests that combine multiple features

    mod integration_tests {
        use super::*;

        #[test]
        fn factorial_recursive() {
            assert_eval("
                ($define! fact
                    ($lambda (n)
                        ($if (eq? n 0)
                            1
                            (* n (fact (- n 1))))))
                (fact 6)
            ", "720");
        }

        #[test]
        fn map_function() {
            assert_eval("
                ($define! map
                    ($lambda (f lst)
                        ($if (null? lst)
                            ()
                            (cons (f (car lst)) (map f (cdr lst))))))
                ($define! double ($lambda (x) (+ x x)))
                (map double (list 1 2 3 4))
            ", "(2 4 6 8)");
        }

        #[test]
        fn filter_function() {
            assert_eval("
                ($define! filter
                    ($lambda (pred lst)
                        ($cond
                            ((null? lst) ())
                            ((pred (car lst))
                                (cons (car lst) (filter pred (cdr lst))))
                            (#t (filter pred (cdr lst))))))
                ($define! positive? ($lambda (x) (eq? (eq? x 0) #f)))
                (filter positive? (list 0 1 0 2 0 3))
            ", "(1 2 3)");
        }

        #[test]
        fn fold_left() {
            assert_eval("
                ($define! fold
                    ($lambda (f init lst)
                        ($if (null? lst)
                            init
                            (fold f (f init (car lst)) (cdr lst)))))
                (fold + 0 (list 1 2 3 4 5))
            ", "15");
        }

        #[test]
        fn custom_operative_if() {
            // Reimplement $if as user-defined operative
            assert_eval("
                ($define! my-if
                    ($vau (test then else) env
                        ($if (eval test env)
                            (eval then env)
                            (eval else env))))
                (my-if (eq? 1 1) (+ 10 20) (+ 100 200))
            ", "30");
        }

        #[test]
        fn let_macro() {
            // Implement $let as a macro using $vau
            assert_eval("
                ($define! $let
                    ($vau (bindings . body) env
                        (eval (cons (list* $lambda (map car bindings) body)
                                   (map cadr bindings))
                              env)))
                ($let ((x 1) (y 2)) (+ x y))
            ", "3");
        }

        #[test]
        fn compose_functions() {
            assert_eval("
                ($define! compose
                    ($lambda (f g)
                        ($lambda (x) (f (g x)))))
                ($define! add1 ($lambda (x) (+ x 1)))
                ($define! double ($lambda (x) (+ x x)))
                ((compose add1 double) 5)
            ", "11");
        }

        #[test]
        fn closure_counter() {
            assert_eval("
                ($define! make-counter
                    ($lambda (n)
                        (list
                            ($lambda () n)
                            ($lambda () ($set! n (+ n 1))))))
                ($define! counter (make-counter 0))
                ($define! get (car counter))
                ($define! inc (car (cdr counter)))
                ($sequence
                    (inc) (inc) (inc)
                    (get))
            ", "3");
        }

        #[test]
        fn y_combinator() {
            // Y combinator for anonymous recursion
            assert_eval("
                ($define! Y
                    ($lambda (f)
                        (($lambda (x) (f ($lambda args (apply (x x) args))))
                         ($lambda (x) (f ($lambda args (apply (x x) args)))))))
                ($define! fact-gen
                    ($lambda (fact)
                        ($lambda (n)
                            ($if (eq? n 0)
                                1
                                (* n (fact (- n 1)))))))
                ((Y fact-gen) 5)
            ", "120");
        }
    }

    // ==================== Additional tests for completeness ====================

    mod quote_tests {
        use super::*;

        #[test]
        fn quote_via_vau() {
            // $quote can be defined as ($vau (x) #ignore x)
            assert_eval("
                ($define! $quote ($vau (x) #ignore x))
                ($quote (+ 1 2))
            ", "(+ 1 2)");
        }

        #[test]
        fn quote_symbol() {
            assert_eval("
                ($define! $quote ($vau (x) #ignore x))
                ($quote x)
            ", "x");
        }
    }

    mod cadr_family_tests {
        use super::*;

        #[test]
        fn cadr_definition() {
            assert_eval("
                ($define! cadr ($lambda ((#ignore x . #ignore)) x))
                (cadr (list 1 2 3))
            ", "2");
        }

        #[test]
        fn caddr_definition() {
            assert_eval("
                ($define! caddr ($lambda ((#ignore #ignore x . #ignore)) x))
                (caddr (list 1 2 3 4))
            ", "3");
        }

        #[test]
        fn caar_definition() {
            assert_eval("
                ($define! caar ($lambda (((x . #ignore) . #ignore)) x))
                (caar (list (list 1 2) 3))
            ", "1");
        }
    }

    mod and_or_tests {
        use super::*;

        #[test]
        fn and_operative() {
            assert_eval("
                ($define! $and
                    ($vau args env
                        ($cond
                            ((null? args) #t)
                            ((null? (cdr args)) (eval (car args) env))
                            ((eval (car args) env) (eval (cons $and (cdr args)) env))
                            (#t #f))))
                ($and #t #t #t)
            ", "#t");

            assert_eval("
                ($define! $and
                    ($vau args env
                        ($cond
                            ((null? args) #t)
                            ((null? (cdr args)) (eval (car args) env))
                            ((eval (car args) env) (eval (cons $and (cdr args)) env))
                            (#t #f))))
                ($and #t #f #t)
            ", "#f");
        }

        #[test]
        fn or_operative() {
            assert_eval("
                ($define! $or
                    ($vau args env
                        ($cond
                            ((null? args) #f)
                            ((null? (cdr args)) (eval (car args) env))
                            ((eval (car args) env) #t)
                            (#t (eval (cons $or (cdr args)) env)))))
                ($or #f #f #t)
            ", "#t");

            assert_eval("
                ($define! $or
                    ($vau args env
                        ($cond
                            ((null? args) #f)
                            ((null? (cdr args)) (eval (car args) env))
                            ((eval (car args) env) #t)
                            (#t (eval (cons $or (cdr args)) env)))))
                ($or #f #f #f)
            ", "#f");
        }
    }

    // ==================== Arithmetic tests ====================
    // Tests for *, /, modulo, and comparison operators

    mod arithmetic_tests {
        use super::*;

        // Multiplication tests
        #[test]
        fn multiply_no_args() {
            // (*) should return 1 (multiplicative identity)
            assert_eval("(*)", "1");
        }

        #[test]
        fn multiply_single() {
            assert_eval("(* 5)", "5");
        }

        #[test]
        fn multiply_two() {
            assert_eval("(* 3 4)", "12");
        }

        #[test]
        fn multiply_many() {
            assert_eval("(* 2 3 4 5)", "120");
        }

        #[test]
        fn multiply_with_zero() {
            assert_eval("(* 5 0 3)", "0");
        }

        #[test]
        fn multiply_negative() {
            assert_eval("(* 3 (- 0 2))", "-6");
        }

        #[test]
        fn multiply_nested() {
            assert_eval("(* (* 2 3) (* 4 5))", "120");
        }

        // Division tests
        #[test]
        fn divide_two() {
            assert_eval("(/ 10 2)", "5");
        }

        #[test]
        fn divide_many() {
            // (/ a b c) = a / b / c
            assert_eval("(/ 100 2 5)", "10");
        }

        #[test]
        fn divide_single() {
            // (/ n) = 1/n (reciprocal)
            assert_eval("(/ 2)", "0");  // Integer division: 1/2 = 0
        }

        #[test]
        fn divide_truncates() {
            // Integer division truncates toward zero
            assert_eval("(/ 7 2)", "3");
        }

        #[test]
        fn divide_by_zero_error() {
            assert_eval_error("(/ 10 0)", "zero");
        }

        // Modulo tests
        #[test]
        fn modulo_basic() {
            assert_eval("(mod 10 3)", "1");
            assert_eval("(mod 15 5)", "0");
        }

        #[test]
        fn modulo_negative() {
            // Behavior with negatives (Kernel follows Scheme convention)
            assert_eval("(mod 10 3)", "1");
        }

        // Comparison predicates
        #[test]
        fn less_than() {
            assert_eval("(<? 1 2)", "#t");
            assert_eval("(<? 2 1)", "#f");
            assert_eval("(<? 2 2)", "#f");
        }

        #[test]
        fn less_than_chain() {
            // Chained comparison: all pairs must satisfy
            assert_eval("(<? 1 2 3 4)", "#t");
            assert_eval("(<? 1 3 2 4)", "#f");
        }

        #[test]
        fn less_or_equal() {
            assert_eval("(<=? 1 2)", "#t");
            assert_eval("(<=? 2 2)", "#t");
            assert_eval("(<=? 3 2)", "#f");
        }

        #[test]
        fn greater_than() {
            assert_eval("(>? 2 1)", "#t");
            assert_eval("(>? 1 2)", "#f");
            assert_eval("(>? 2 2)", "#f");
        }

        #[test]
        fn greater_or_equal() {
            assert_eval("(>=? 2 1)", "#t");
            assert_eval("(>=? 2 2)", "#t");
            assert_eval("(>=? 1 2)", "#f");
        }

        #[test]
        fn numeric_equal() {
            assert_eval("(=? 5 5)", "#t");
            assert_eval("(=? 5 6)", "#f");
            assert_eval("(=? 1 1 1 1)", "#t");
            assert_eval("(=? 1 1 2 1)", "#f");
        }

        // Zero/positive/negative predicates
        #[test]
        fn zero_predicate() {
            assert_eval("(zero? 0)", "#t");
            assert_eval("(zero? 1)", "#f");
            assert_eval("(zero? (- 0 1))", "#f");
        }

        #[test]
        fn positive_predicate() {
            assert_eval("(positive? 5)", "#t");
            assert_eval("(positive? 0)", "#f");
            assert_eval("(positive? (- 0 5))", "#f");
        }

        #[test]
        fn negative_predicate() {
            assert_eval("(negative? (- 0 5))", "#t");
            assert_eval("(negative? 0)", "#f");
            assert_eval("(negative? 5)", "#f");
        }

        #[test]
        fn odd_even_predicates() {
            assert_eval("(odd? 3)", "#t");
            assert_eval("(odd? 4)", "#f");
            assert_eval("(even? 4)", "#t");
            assert_eval("(even? 3)", "#f");
        }

        #[test]
        fn abs_function() {
            assert_eval("(abs 5)", "5");
            assert_eval("(abs (- 0 5))", "5");
            assert_eval("(abs 0)", "0");
        }

        #[test]
        fn min_max() {
            assert_eval("(min 3 1 4 1 5)", "1");
            assert_eval("(max 3 1 4 1 5)", "5");
        }
    }

    // ==================== List operation tests ====================

    mod list_operation_tests {
        use super::*;

        #[test]
        fn length_empty() {
            assert_eval("(length ())", "0");
        }

        #[test]
        fn length_list() {
            assert_eval("(length (list 1 2 3 4 5))", "5");
        }

        #[test]
        fn list_ref() {
            assert_eval("(list-ref (list 10 20 30 40) 0)", "10");
            assert_eval("(list-ref (list 10 20 30 40) 2)", "30");
        }

        #[test]
        fn list_ref_out_of_bounds() {
            assert_eval_error("(list-ref (list 1 2 3) 10)", "bounds");
        }

        #[test]
        fn list_tail_basic() {
            assert_eval("(list-tail (list 1 2 3 4 5) 2)", "(3 4 5)");
        }

        #[test]
        fn list_tail_zero() {
            assert_eval("(list-tail (list 1 2 3) 0)", "(1 2 3)");
        }

        #[test]
        fn list_tail_full() {
            assert_eval("(list-tail (list 1 2 3) 3)", "()");
        }

        #[test]
        fn append_two_lists() {
            assert_eval("(append (list 1 2) (list 3 4))", "(1 2 3 4)");
        }

        #[test]
        fn append_empty() {
            assert_eval("(append () (list 1 2))", "(1 2)");
            assert_eval("(append (list 1 2) ())", "(1 2)");
        }

        #[test]
        fn append_multiple() {
            assert_eval("(append (list 1) (list 2) (list 3))", "(1 2 3)");
        }

        #[test]
        fn reverse_list() {
            assert_eval("(reverse (list 1 2 3 4))", "(4 3 2 1)");
        }

        #[test]
        fn reverse_empty() {
            assert_eval("(reverse ())", "()");
        }

        #[test]
        fn member_found() {
            assert_eval("(member 2 (list 1 2 3))", "(2 3)");
        }

        #[test]
        fn member_not_found() {
            assert_eval("(member 5 (list 1 2 3))", "#f");
        }

        #[test]
        fn assoc_found() {
            assert_eval("(assoc 2 (list (list 1 10) (list 2 20) (list 3 30)))", "(2 20)");
        }

        #[test]
        fn assoc_not_found() {
            assert_eval("(assoc 5 (list (list 1 10) (list 2 20)))", "#f");
        }
    }

    // ==================== Environment operation tests ====================

    mod environment_tests {
        use super::*;

        #[test]
        fn binds_predicate_true() {
            assert_eval("
                ($define! x 42)
                ($binds? (get-current-environment) x)
            ", "#t");
        }

        #[test]
        fn binds_predicate_false() {
            assert_eval("
                ($binds? (make-environment) nonexistent-symbol)
            ", "#f");
        }

        #[test]
        fn get_current_environment() {
            assert_eval("
                ($define! x 10)
                (eval (quote x) (get-current-environment))
            ", "10");
        }

        #[test]
        fn make_environment_with_parent() {
            assert_eval("
                ($define! parent-env (get-current-environment))
                ($define! x 42)
                ($define! child-env (make-environment parent-env))
                (eval (quote x) child-env)
            ", "42");
        }

        #[test]
        fn make_environment_multiple_parents() {
            assert_eval("
                ($define! env1 (make-environment))
                ($define! env2 (make-environment))
                (eval (quote ($define! x 1)) env1)
                (eval (quote ($define! y 2)) env2)
                ($define! combined (make-environment env1 env2))
                (+ (eval (quote x) combined) (eval (quote y) combined))
            ", "3");
        }
    }

    // ==================== $let family tests ====================

    mod let_tests {
        use super::*;

        #[test]
        fn let_basic() {
            assert_eval("
                ($let ((x 1) (y 2))
                    (+ x y))
            ", "3");
        }

        #[test]
        fn let_empty_bindings() {
            assert_eval("($let () 42)", "42");
        }

        #[test]
        fn let_shadows_outer() {
            assert_eval("
                ($define! x 1)
                ($let ((x 10))
                    x)
            ", "10");
        }

        #[test]
        fn let_outer_still_accessible_after() {
            assert_eval("
                ($define! x 1)
                ($let ((x 10)) x)
                x
            ", "1");
        }

        #[test]
        fn let_bindings_not_recursive() {
            // In $let, bindings can't see each other
            assert_eval_error("
                ($let ((x 1) (y x))
                    y)
            ", "Lookup");
        }

        #[test]
        fn let_star_sequential() {
            // In $let*, bindings are sequential
            assert_eval("
                ($let* ((x 1) (y (+ x 1)) (z (+ y 1)))
                    z)
            ", "3");
        }

        #[test]
        fn letrec_recursive() {
            // $letrec allows recursive bindings
            assert_eval("
                ($letrec ((fact ($lambda (n)
                                    ($if (eq? n 0)
                                        1
                                        (* n (fact (- n 1)))))))
                    (fact 5))
            ", "120");
        }

        #[test]
        fn letrec_mutual_recursion() {
            assert_eval("
                ($letrec ((even? ($lambda (n)
                                    ($if (eq? n 0) #t (odd? (- n 1)))))
                          (odd? ($lambda (n)
                                    ($if (eq? n 0) #f (even? (- n 1))))))
                    (even? 10))
            ", "#t");
        }

        #[test]
        fn let_destructuring() {
            assert_eval("
                ($let (((a b) (list 1 2)))
                    (+ a b))
            ", "3");
        }
    }

    // ==================== not and boolean operation tests ====================

    mod boolean_tests {
        use super::*;

        #[test]
        fn not_true() {
            assert_eval("(not #t)", "#f");
        }

        #[test]
        fn not_false() {
            assert_eval("(not #f)", "#t");
        }

        #[test]
        fn not_error_non_boolean() {
            assert_eval_error("(not 42)", "boolean");
        }

        #[test]
        fn and_applicative_empty() {
            assert_eval("(and?)", "#t");
        }

        #[test]
        fn and_applicative_all_true() {
            assert_eval("(and? #t #t #t)", "#t");
        }

        #[test]
        fn and_applicative_one_false() {
            assert_eval("(and? #t #f #t)", "#f");
        }

        #[test]
        fn or_applicative_empty() {
            assert_eval("(or?)", "#f");
        }

        #[test]
        fn or_applicative_all_false() {
            assert_eval("(or? #f #f #f)", "#f");
        }

        #[test]
        fn or_applicative_one_true() {
            assert_eval("(or? #f #t #f)", "#t");
        }
    }

    // ==================== combiner? and related tests ====================

    mod combiner_tests {
        use super::*;

        #[test]
        fn combiner_predicate_operative() {
            assert_eval("(combiner? $if)", "#t");
        }

        #[test]
        fn combiner_predicate_applicative() {
            assert_eval("(combiner? cons)", "#t");
        }

        #[test]
        fn combiner_predicate_non_combiner() {
            assert_eval("(combiner? 42)", "#f");
            assert_eval("(combiner? ())", "#f");
        }
    }

    // ==================== copy-es tests ====================

    mod copy_es_tests {
        use super::*;

        #[test]
        fn copy_es_makes_mutable_copy() {
            assert_eval("
                ($define! original (list 1 2 3))
                ($define! copied (copy-es original))
                (set-car! copied 99)
                (car original)
            ", "1");
        }

        #[test]
        fn copy_es_immutable_is_immutable() {
            assert_eval_error("
                ($define! copied (copy-es-immutable (list 1 2 3)))
                (set-car! copied 99)
            ", "immutable");
        }
    }

    // ==================== map and for-each tests ====================

    mod map_tests {
        use super::*;

        #[test]
        fn map_single_list() {
            assert_eval("
                ($define! double ($lambda (x) (* x 2)))
                (map double (list 1 2 3 4))
            ", "(2 4 6 8)");
        }

        #[test]
        fn map_empty_list() {
            assert_eval("(map ($lambda (x) x) ())", "()");
        }

        #[test]
        fn map_multiple_lists() {
            assert_eval("(map + (list 1 2 3) (list 10 20 30))", "(11 22 33)");
        }

        #[test]
        fn for_each_side_effects() {
            assert_eval("
                ($define! sum 0)
                (for-each ($lambda (x) ($set! sum (+ sum x))) (list 1 2 3 4))
                sum
            ", "10");
        }

        #[test]
        fn for_each_returns_inert() {
            assert_eval("
                (for-each ($lambda (x) x) (list 1 2 3))
            ", "#inert");
        }
    }

    // ==================== filter and reduce tests ====================

    mod filter_reduce_tests {
        use super::*;

        #[test]
        fn filter_basic() {
            assert_eval("
                ($define! positive? ($lambda (x) (>? x 0)))
                (filter positive? (list (- 0 1) 0 1 2 (- 0 2) 3))
            ", "(1 2 3)");
        }

        #[test]
        fn filter_empty_result() {
            assert_eval("(filter ($lambda (x) #f) (list 1 2 3))", "()");
        }

        #[test]
        fn reduce_sum() {
            assert_eval("(reduce + 0 (list 1 2 3 4 5))", "15");
        }

        #[test]
        fn reduce_empty_list() {
            assert_eval("(reduce + 0 ())", "0");
        }

        #[test]
        fn reduce_product() {
            assert_eval("(reduce * 1 (list 1 2 3 4 5))", "120");
        }
    }

    // ==================== string tests ====================

    mod string_tests {
        use super::*;

        #[test]
        fn string_predicate() {
            assert_eval("(string? \"hello\")", "#t");
            assert_eval("(string? 42)", "#f");
        }

        #[test]
        fn string_equal() {
            assert_eval("(equal? \"hello\" \"hello\")", "#t");
            assert_eval("(equal? \"hello\" \"world\")", "#f");
        }
    }
}

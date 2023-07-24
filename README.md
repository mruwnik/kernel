# Kernel

This is an implementation of [Kernel](https://web.cs.wpi.edu/~jshutt/kernel.html) in Rust. Or at least that's the goal - it'll probably not have everything for it to be a fully fledged implementation.

## Execution

Run `cargo run "<expr>"`, where `<expr>` is a valid kernel expression. Some examples would be:

* `cargo run '1'`
* `cargo run '(+ 1 2 3 4)'`
* `cargo run '(list (list 1 2 3) (+ 3 4))'`
* `cargo run '(eval (cons (car (cons + ())) (list 3 4)) (make-environment))'`

## Known issues:
* the circular lists magic thingy for equals?
* tidy up extract_list
* add lots of tests for extract_list
* handle chars

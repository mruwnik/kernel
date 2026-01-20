# Kernel

A partial implementation of [Kernel](https://web.cs.wpi.edu/~jshutt/kernel.html) in Rust. Kernel is a Lisp dialect that distinguishes between operatives (which receive unevaluated arguments) and applicatives (which receive evaluated arguments).

## Building

```bash
cargo build
```

## Running

```bash
cargo run -- '<expr>'
```

where `<expr>` is a valid Kernel expression.

### Examples

```bash
cargo run -- '1'                                    # => 1
cargo run -- '(+ 1 2 3 4)'                          # => 10
cargo run -- '(- 10 3 2)'                           # => 5
cargo run -- '(list 1 2 3)'                         # => (1 2 3)
cargo run -- '(cons 1 2)'                           # => (1 . 2)
cargo run -- '(car (list 1 2 3))'                   # => 1
cargo run -- '(cdr (list 1 2 3))'                   # => (2 3)
cargo run -- '($if #t 1 2)'                         # => 1
cargo run -- '(boolean? #t)'                        # => #t
cargo run -- '(eq? 1 1)'                            # => #t
```

## Implemented Features

### Primitives

| Form | Type | Description |
|------|------|-------------|
| `$if` | Operative | Conditional: `($if test consequent alternative)` |
| `$define!` | Operative | Define bindings: `($define! symbol expr)` |
| `eval` | Applicative | Evaluate expression in environment |
| `cons` | Applicative | Construct pair |
| `car` | Applicative | First element of pair |
| `cdr` | Applicative | Rest of pair |
| `list` | Applicative | Construct list from arguments |
| `set-car!` | Applicative | Mutate car of pair |
| `set-cdr!` | Applicative | Mutate cdr of pair |
| `copy-es-immutable` | Applicative | Copy pair structure as immutable |
| `make-environment` | Applicative | Create new environment |
| `eq?` | Operative | Identity equality |
| `equal?` | Operative | Structural equality |

### Type Predicates

`boolean?`, `applicative?`, `operative?`, `inert?`, `ignore?`, `null?`, `env?`, `number?`, `pair?`, `string?`, `symbol?`

### Arithmetic

| Form | Description |
|------|-------------|
| `+` | Addition (variadic) |
| `-` | Subtraction (variadic) |

### Data Types

- **Booleans**: `#t`, `#f`
- **Numbers**: integers and floats
- **Strings**: `"hello"`
- **Symbols**: `foo`, `my-symbol`
- **Constants**: `#inert`, `#ignore`, `()` (null)
- **Pairs/Lists**: `(1 . 2)`, `(1 2 3)`
- **Environments**: first-class environments

## Testing

```bash
cargo test
```

312 tests covering lexing, parsing, and evaluation.

## Known Issues

- Character literals not yet implemented

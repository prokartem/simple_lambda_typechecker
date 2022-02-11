# Type checker for simply typed lambda calculus

## Syntax

### Terms
`t` — term 
- Variable `x`
- Abstraction `lambda x : T. t`
- Application `t $ t`

### Types
`T` — type
- Arrow `T -> T`

### Additional
- Comments `{- some text -}`
- Term separator (if placed in the end of file, parser raises error) `;`

### Correct input
```
{- simple term -}
lambda x : Y -> X. lambda y : Y. x $ y;

{- complex term with intendation -}
lambda x : A -> B -> G. 
    lambda y : A -> B. 
        lambda z : A. 
            x $ z $ (y $ z)
```

## Install
```
git clone https://github.com/prokartem/simple_lambda_typechecker.git
cd simple_lambda_typechecker
curl -sSL https://get.haskellstack.org/ | sh -s - -f
stack build
```

## Usage
Available options:
 - Input file `-f`, `--file FILENAME`
 - Read from stdin `--stdin`                     
 - Show this help text `-h`, `--help`

`stack exec -- lambda-calculus-exe ((-f|--file FILENAME) | --stdin)`

## Test
`stack test`


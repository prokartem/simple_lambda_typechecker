# Type checker for simply typed lambda calculus

### Install
```
git clone https://github.com/prokartem/simple_lambda_typechecker.git
cd simple_lambda_typechecker
curl -sSL https://get.haskellstack.org/ | sh -s - -f
stack build
```

### Usage
Available options:
 - Input file `-f`, `--file FILENAME`
 - Read from stdin `--stdin`                     
 - Show this help text `-h`, `--help`

`stack exec -- lambda-calculus-exe ((-f|--file FILENAME) | --stdin)`

### Test
`stack test`


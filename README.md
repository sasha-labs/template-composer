# Template Composer
Template Composer is a program designed to create text templates in any language. It enables users to compose files by nesting imports, allowing them to abstract away the complexities of the underlying code and template setup. This way, only the values that need to be adjusted are exposed.

# Usage
The ```job.cfg``` file requires three values:
1. the instruction file
2. the target file (any format)
3. the output location (any format)

## Instruction File
The instruction file is processed sequentially, from top to bottom, as a series of expressions. This allows for template composition. Each expression follows a uniform format:

```
<value><operator>"<value>"
```

Operations proceed from left to right, e.g. ```value1``` is replaced by ```value2```:

```
<value1>:="<value2>"
```

### Commenting
Comments can be added by prefixing them with ```--```:

```
-- Section #1
...

-- Section #2
...
```

### Replace Operator
The replace operator ```:=``` replaces all instances of the left-hand value with the right-hand value:

```
./target.txt

Hello World! Hello World! Hello World!
```
```
./instructions

Hello:="Bye"
```
```
./output.txt

Bye World! Bye World! Bye World!
```

### Number Operator
The number operator ```:_``` replaces all instances of the left-hand value with the right-hand value and appends a number, starting from 1:

```
./target.txt

Alex likes fruit, fruit, and fruit.
```
```
./instructions

fruit:_"fruit-"
fruit-1:="oranges"
fruit-2:="apples"
fruit-2:="grapes"
```
```
./output.txt

Alex likes oranges, apples, and grapes.
```

### Replace With Contents Operator
The replace with contents operator ```<-``` works similarly to the replace operator, but the replacement value is read from an external file:

```
./target.txt

Alex likes fruits. <point>
```
```
./arbitraryfile.txt

But he dislikes most vegetables.
```
```
./instructions

<point><-"./arbitraryfile.txt"
```
```
./output.txt

Alex likes fruits. But he dislikes most vegetables.
```

### Import Operator
The import operator ```<=``` reads and executes the contents of another instruction file before continuing with the original one. The left-hand value is arbitrary:

```
./instructions

<point1>:="my value"
<point2>:="my value"
import<="./anotherFile"
value:="string"
```
```
./anotherFile

value:_"value "
```
is equivalent to:
```
<point1>:="my value"
<point2>:="my value"
value:_"value "
value:="string"
```

## Target File
The target file can be any text file. The user should define the injection points where new content will be inserted. These points should be unique (or repeated if the same content needs to be inserted multiple times):

```
//...
This is where more text will be inserted: <myInjectionPoint>

And it will also be here <myInjectionPoint>!
//...
```
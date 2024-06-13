# `hgit init`

1. given $CWD has no .git folder, when `hgit init` is run, then

- $CWD/.git/ is created
- $CWD/.git/HEAD is created
- $CWD/.git/HEAD file is initialized with "ref: refs/heads/master"
- $CWD/.git/config file is created
- $CWD/.git/objects/ is created
- $CWD/.git/objects/info is created
- $CWD/.git/objects/pack is created
- $CWD/.git/refs/ is created
- $CWD/.git/refs/heads/ is created
- $CWD/.git/refs/tags/ is created

2. given $CWD has .git folder, when `hgit init` is run, then print error to stdout

3. given $CWD has no {dir} folder, when `hgit init {dir}` is run, then

- $CWD/{dir} is created
- $CWD/{dir}/.git/ is created
- $CWD/{dir}/.git/HEAD is created
- $CWD/{dir}/.git/HEAD file is initialized with "ref: refs/heads/master"
- $CWD/{dir}/.git/config file is created
- $CWD/{dir}/.git/objects/ is created
- $CWD/{dir}/.git/objects/info is created
- $CWD/{dir}/.git/objects/pack is created
- $CWD/{dir}/.git/refs/ is created
- $CWD/{dir}/.git/refs/heads/ is created
- $CWD/{dir}/.git/refs/tags/ is created

4. Given
   i. $CWD has {dir} folder
   ii. $CWD/.git doesn't exist
   When `hgit init {dir}` is run
   Then,

- $CWD/{dir} is created
- $CWD/{dir}/.git/ is created
- $CWD/{dir}/.git/HEAD is created
- $CWD/{dir}/.git/HEAD file is initialized with "ref: refs/heads/master"
- $CWD/{dir}/.git/config file is created
- $CWD/{dir}/.git/objects/ is created
- $CWD/{dir}/.git/objects/info is created
- $CWD/{dir}/.git/objects/pack is created
- $CWD/{dir}/.git/refs/ is created
- $CWD/{dir}/.git/refs/heads/ is created
- $CWD/{dir}/.git/refs/tags/ is created

5. Given $CWD has {dir} folder and $CWD/.git doesn't exist, when `hgit init {dir}` is run, then print error msg to stdout

# `hgit hash-object`

1. Given $CWD/.git exists, when `hgit hash-object` is run, then do nothing

2. Given

- $CWD/.git exists
- $CWD/testy exist
- content of is $CWD/testy "hello world"
- $CWD/testy has object id 3b18e512dba79e4c8300dd08aeb37f8e728b8dad,

when `hgit hash-object testy` is run, then

- stdout prints 3b18e512dba79e4c8300dd08aeb37f8e728b8dad

3. Given

- $CWD/.git exists
- $CWD/testy exist
- content of is $CWD/testy "hello world"
- $CWD/testy has object id 3b18e512dba79e4c8300dd08aeb37f8e728b8dad,

when `hgit hash-object -w testy` is run, then

- stdout prints 3b18e512dba79e4c8300dd08aeb37f8e728b8dad
- $CWD/.git/objects/3b/18e512dba79e4c8300dd08aeb37f8e728b8dad is created

# `hgit cat-file`

1. Given

- $CWD/.git/objects/3b/18e512dba79e4c8300dd08aeb37f8e728b8dad exists, and
- it has binary content "78 01 4B CA C9 4F 52 30 34 62 C8 48 CD C9 C9 57 28 CF 2F CA 49 E1 02 00 44 11 06 89"

when `hgit cat-file -t 3b18e512dba79e4c8300dd08aeb37f8e728b8dad` is run, then "blob" is displayed in stdout

2. when `hgit cat-file (-t|-p) <invalidHash>` then stdout produces an error
   where

- invalidHash is any hash not 40 char long or contains non-hexadecimal digits

3. given $CWD/.git/objects/3b/18e512dba79e4c8300dd08aeb37f8e728b8dad doesn't exist
   when `hgit cat-file (-t|-p) 3b18e512dba79e4c8300dd08aeb37f8e728b8dad` is run, then error is printed in stdout

4. Given

- $CWD/.git/objects/3b/18e512dba79e4c8300dd08aeb37f8e728b8dad exists, and
- it has binary content "78 01 4B CA C9 4F 52 30 34 62 C8 48 CD C9 C9 57 28 CF 2F CA 49 E1 02 00 44 11 06 89"

when `hgit cat-file -p 3b18e512dba79e4c8300dd08aeb37f8e728b8dad` is run, then "hello world" is displayed in stdout

5. [test when the contents of the file is corrupted]

# `hgit update-index`

1. given

- $CWD/.git exists
- $CWD/testy exist
- content of is $CWD/testy "hello world"

when `hgit update-index --add testy` is run, then

- $CWD/.git/index is created
- $CWD/.git/objects/3b/18e512dba79e4c8300dd08aeb37f8e728b8dad is created if not already

# `hgit write-tree`

given

- `hgit init` is executed
- $CWD/testy exist
- content of $CWD/testy is "hello world"
- `hgit update-index --add testy` is executed

when `hgit write-tree` is run, then

- da34c8e94b6666fe041e117eec514b87430db516 is printed in stdout
- $CWD/.git/objects/da/34c8e94b6666fe041e117eec514b87430db516 is created

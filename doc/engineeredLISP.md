# Common LISP 工程化

## 用 sbcl, asdf 和 cl-launch 编写可分发的 lisp 程序（一）

如果你认为看完并且看懂了这五本书：

1. 《Common Lisp: A Gentle Introduction to Symbolic Computation》
1. 《Common Lisp: The Language 2nd》
1. 《On Lisp》
1. 《Practical Common Lisp》
1. 《计算机程序的构造和解释》

就能写出完整的 Common Lisp 程序来，那就大错特错了
(不过要看完上述五本书仍然是一件很艰难而且很耗时的事，
这就是为何成为 Lisp 程序员更难一些的主要原因：语言规模大、并且有自己独特的编程风格)。

事实上对于 C 程序员来说，上述五本书基本上只相当于
K&R 的《The C Programming Language》而已，
正如写一个完整的 C 程序还需要诸如编译、调试、Makefile(或者全套的autotools)以及
各种各样的第三方库那样，编写完整的 Common Lisp 程序
决不仅仅是打开一个 lisp 的交互环境然后输入一个
(format t "Hello, world!~%") 那么简单。

要想让自己写的 lisp 程序像其他语言编写的程序那样运行，我们要解决的是两个问题：

1. 用一个类似 Makefile 的系统来帮助编译多文件组成的源代码，以及方便地引用其他 Lisp 软件包。
1. 将 lisp 基于交互的运行模式转化为可在操作系统命令行上直接运行的独立可执行程序。

下面我将给出一个完整的代码示例，以实现一个普通的命令行程序，
他能输出 hello world，但是如果提供了命令行参数例如 "a b c"，
它就会根据每个参数生成类似这样的输出：

```
hello a
hello b
hello c
```

另外，我的程序也有自己的默认配置参数，这个参数决定了当我不给出任何命令行参数时，
程序在 "hello" 字样后面输出的是 "world". 以下是全部的过程：

1. 确保你使用的是 Debian 或者 Ubuntu 系统，否则下列操作不可能全部照搬。
    然后至少要安装 sbcl 和 cl-launch 软件包。
1. 在文件系统里给这个命名为 hello 的项目建一个项目目录。
    (对我自己来说，放在 /home/binghe/lisp/src/hello 目录下了)
1. 在项目目录里(以下如果没有特别指明的话，所有文件都是建立在项目目录里的)
    建一个 hello.asd 文件，作为 asdf 自动编译系统的加载入口，代码如下：

  ```lisp
  ;;;; -*- Lisp -*-

  (defpackage :hello-system (:use #:asdf #:cl))
  (in-package :hello-system)

  (defsystem hello
    :name "Hello World"
    :version "0.1"
    :author "Chun Tian (binghe)"
    :depends-on ()
    :components ((:file "package")
             (:file "config" :depends-on ("package"))
             (:file "hello" :depends-on ("config"))))
  ```

  上述代码有些类似于 Makefile，
  它首先定义了一个新的名为 hello-system 的 package，使用了 asdf 包，
  以便在这个独立的包里可以定义一个独立的 system，这是个 asdf 建议的好习惯，
  这样我们所有的代码将位于自己独立的 package 中，
  如果需要从 lisp 环境中清理出去就非常方便。

  接下来的 defsystem 宏就定义了整个项目的代码结构，以及一些无用的附加信息。
  重要的部分是 components，它定义了三个有明确依赖关系的
  源代码文件 package.lisp, config.lisp 和 hello.lisp，
  一般而言，对于稍具规模的正规 lisp 程序，至少需要三个代码文件：
  一个用来定义 package，一个存放配置信息，一个放实际的业务逻辑代码。
  如果此项目依赖于其他 asdf 格式的 lisp 软件包，那么写在 depends-on 段里即可。

1. 定义 package，与 hello-system 不同，binghe.hello 这个 package 是用来实际放代码的。

  以下是 package.lisp 文件的内容：

  ```lisp
  (in-package :hello-system)

  (defpackage binghe.hello
    (:nicknames hello)
    (:use #:cl)
    (:export main *default-name*))
  ```

  我定义了一个 binghe.hello 包，并且给这个较长的包名称指定了一个较短的昵称 hello，
  然后用 use 段设置这个包可以访问所有标准 Common Lisp 符号，
  根据 Lisp 标准他们位于 common-lisp 包里，这个包的昵称是 cl。
  最后我导出了两个 hello 包里的符号作为外部接口。

1. 定义配置代码文件，用于指定默认输出的名字：(这个文件在如此短小的项目里毫无必要，只是出于演示目的)

  ```lisp
  (in-package :hello)

  (defvar *default-name* "world")
  ```

1. 定义核心代码文件：

  ```lisp
  (in-package :hello)

  (defun main (args)
    (if (null args)
        (format t "hello ~A~%" *default-name*)
        (hello args)))

  (defun hello (names)
    (when names
      (format t "hello ~A~%" (car names))
      (hello (cdr names))))
  ```

  上述代码里有两个函数定义，main 函数是整个程序的入口，入口参数是一个列表，
  如果列表为空的话就产生默认输出然后程序结束，
  否则就调用另一个函数 hello 来实际产生针对每个列表元素的输出，
  注意到这个函数我采用了尾递归的写法，这在 lisp 程序里是非常自然的编程风格，
  完全没有任何性能折损而且相比循环结构节省了显式的循环变量。

1. 实际上如果代码正确的话，现在在这个目录里运行 sbcl，然后输入 (clc:clc-require :hello) 就可以编译这个项目了：

  ```lisp
  binghe@localhost:~/lisp/src/hello$ ls -l

  -rw-r--r-- 1 binghe staff  53 2006-10-23 00:50 config.lisp
  -rw-r--r-- 1 binghe staff 326 2006-10-23 00:48 hello.asd
  -rw-r--r-- 1 binghe staff 226 2006-10-23 00:56 hello.lisp
  -rw-r--r-- 1 binghe staff 161 2006-10-23 01:00 Makefile
  -rw-r--r-- 1 binghe staff 122 2006-10-23 00:59 package.lisp

  binghe@localhost:~/lisp/src/hello$ sbcl
  This is SBCL 0.9.17, an implementation of ANSI Common Lisp.
  More information about SBCL is available at <http://www.sbcl.org/>.

  SBCL is free software, provided as is, with absolutely no warranty.
  It is mostly in the public domain; some portions are provided under
  BSD-style licenses.  See the CREDITS and COPYING files in the
  distribution for more information.
  ; in: LAMBDA NIL
  ;     (SB-KERNEL:FLOAT-WAIT)

  ; note: deleting unreachable code

  ; compilation unit finished
  ;   printed 1 note
  CL-USER(1): (clc:clc-require :hello)

  CL-USER(2): (quit)
  binghe@localhost:~/lisp/src/hello$ sbcl
  This is SBCL 0.9.17, an implementation of ANSI Common Lisp.
  More information about SBCL is available at <http://www.sbcl.org/>.

  SBCL is free software, provided as is, with absolutely no warranty.
  It is mostly in the public domain; some portions are provided under
  BSD-style licenses.  See the CREDITS and COPYING files in the
  distribution for more information.
  ; in: LAMBDA NIL
  ;     (SB-KERNEL:FLOAT-WAIT)

  ; note: deleting unreachable code

  ; compilation unit finished
  ;   printed 1 note
  CL-USER(1): (clc:clc-require :hello)

  ;;; Please wait, recompiling library...
  ; compiling file "/home/binghe/lisp/src/hello/package.lisp" (written 23 OCT 2006 12:59:11 AM):
  ; compiling (IN-PACKAGE :HELLO-SYSTEM)
  ; compiling (DEFPACKAGE BINGHE.HELLO ...)

  ; /var/cache/common-lisp-controller/1000/sbcl/local/home/binghe/lisp/src/hello/package.fasl written
  ; compilation finished in 0:00:00
  ; compiling file "/home/binghe/lisp/src/hello/config.lisp" (written 23 OCT 2006 12:50:45 AM):
  ; compiling (IN-PACKAGE :HELLO)
  ; compiling (DEFVAR *DEFAULT-NAME* ...)

  ; /var/cache/common-lisp-controller/1000/sbcl/local/home/binghe/lisp/src/hello/config.fasl written
  ; compilation finished in 0:00:00
  ; compiling file "/home/binghe/lisp/src/hello/hello.lisp" (written 23 OCT 2006 12:56:33 AM):
  ; compiling (IN-PACKAGE :HELLO)
  ; compiling (DEFUN MAIN ...)
  ; compiling (DEFUN HELLO ...)

  ; /var/cache/common-lisp-controller/1000/sbcl/local/home/binghe/lisp/src/hello/hello.fasl written
  ; compilation finished in 0:00:00

  CL-USER(2): (hello:main nil)
  hello world

  CL-USER(3): (hello:main '("binghe" "netease" "sa"))
  hello binghe
  hello netease
  hello sa
  ```

  注意到编译成功之后我立即测试了代码，输出看起来是正确的。
  由于 lisp 环境的初始所在包是 cl-user，
  为了引用其他包的函数我必须将包名也作为函数名的一部分来使用：(hello:main ...)
  或者 (binghe.hello:main ...)，
  测试结果对于空列表(nil) 和非空列表都是正确的，
  函数最后输出的 NIL 是函数的返回值，
  这个值只在交互环境下以求值为目的运行函数时才有意义，
  而我们调用 main 函数实际上是为了得到副作用(标准输出)而不是函数值。

1. 下面我们用 cl-launch 来生成可以在操作系统环境下执行的独立程序，为了方便起见，我使用了 make，做了一个真正的 Makefile:

  ```makefile
  hello: hello.asd *.lisp
      cl-launch -d hello.core -s hello -l sbcl -o hello --init "(hello:main cl-launch:*arguments*)"

  clean:
      rm -f *~ hello hello.core *.fasl
  ```

  注意 cl-launch 的各个参数，其中 -d 让 lisp 环境 dump 出一个完整的 core 文件以便加速程序的初始加载，这个参数对于大量引用了外部 lisp 包的情况特别有用，但对我们来说纯粹是浪费，因为 sbcl 会 dump 出一个 20多兆的 core 文件来。
  -s 参数用来加载 asdf 包，也就是我们刚刚做的 hello 包，借此参数 cl-launch 就能加载我们所有的代码了。-l 参数设置使用的 lisp 平台类型，cl-launch 还支持 cmucl 和 clisp 但是我们现在不用。-o 设置了最后输出的可执行脚本名。
  --init 参数最重要，设置了程序的入口点。cl-launch 提供了一个命令行参数的约定入口 cl-launch:*arguments* 以实现各种不同的 lisp 平台的统一命令行参数支持，我要明确地让这些命令行参数进入我们自己写的 main 函数，并且这个函数首先执行。C 语言实际上也有类似机制，那就是 main() 函数，实际上 C 编译器把这个初始工作给偷偷做掉了并且不允许用户修改这一行为，Lisp 则灵活一些。

  于是我运行 make 命令，最后在项目目录里得到的文件如下：

  ```
  cl-launch -d hello.core -s hello -l sbcl -o hello --init "(hello:main cl-launch:*arguments*)"
  [undoing binding stack and other enclosing state... done]
  [saving current Lisp image into /home/binghe/lisp/src/hello/hello.core:
  writing 1912 bytes from the read-only space at 0x01000000
  writing 1936 bytes from the static space at 0x05000000
  writing 25640960 bytes from the dynamic space at 0x09000000
  done]
  binghe@localhost:~/lisp/src/hello$ ls -l
  总计 25116
  -rw-r--r-- 1 binghe staff       53 2006-10-23 00:50 config.lisp
  -rwxr-xr-x 1 binghe staff     8054 2006-10-23 01:32 hello
  -rw-r--r-- 1 binghe staff      328 2006-10-23 01:00 hello.asd
  -rw-r--r-- 1 binghe staff 25681928 2006-10-23 01:32 hello.core
  -rw-r--r-- 1 binghe staff      226 2006-10-23 00:56 hello.lisp
  -rw-r--r-- 1 binghe staff      161 2006-10-23 01:00 Makefile
  -rw-r--r-- 1 binghe staff      122 2006-10-23 00:59 package.lisp
  ```

  注意，多了两个文件。hello 是带有可执行标志位的脚本，hello.core 是 sbcl dump 出来的 corp 文件，内含整个 Common Lisp 的语言实现以及我们自己写的所有程序的二进制形式。实际上，商业 Lisp 实现与开源 Lisp 实现的主要区别就在这里，商业 Lisp 实现能 dump 出一个小得多的真正的可执行文件，其中只含有我们的程序用得着的那些 Common Lisp 语言实现部分，其他没有用的东西在 dump 的时候直接扔掉了。

  现在我们可以测试这个编译成果了：

  ```
  binghe@localhost:~/lisp/src/hello$ ./hello
  hello world
  binghe@localhost:~/lisp/src/hello$ ./hello a b c
  hello a
  hello b
  hello c
  ```

现在，hello 和 hello.core 文件可以分发到其他安装了 sbcl 和 cl-launch 的 Debian 系统下运行了。不过，还有很明显的需求没有满足：

1. 能得到一个在没有 sbcl 的 Linux 系统(包括非 Debian 的系统) 下也能运行的可执行程序吗？
1. 能得到一个单一的可执行文件，完全脱离脚本吗？

这两个问题都是可以解决的，但是需要更多的工作，我将在下一篇文章里介绍。

## 用 sbcl, asdf 和 cl-launch 编写可分发的 lisp 程序（二）

最近心血来潮，想看看我的博客访问量怎样，又哪些人访问，
惊奇地发现我的一些 Lisp 方面的文章被 OCaml China (http://ocaml.cn/node/213) 收录了，
欣喜之余，发现我还欠各位观众至少一个下文，那就补上喽^\_^

国际形势风云变换，
今天再写《用 sbcl, asdf 和 cl-launch 编写可分发的 lisp 程序》就跟当时的背景不太一样了：
SBCL 已经从 0.9.x 升级到了1.0，
cl-launch 也已经从 1.x 升级到了 2.03，
下面我将借助更好的工具回答上次遗留的两个问题：

1. 能得到一个在没有 sbcl 的 Linux 系统(包括非 Debian 的系统) 下也能运行的可执行程序吗？
1. 能得到一个单一的可执行文件，完全脱离脚本吗？

SBCL 从 0.9.10 以后，在 SAVE-LISP-AND-DIE 函数中新增一个 EXECUTABLE 关键字参数，
借助这个参数可以导出含有用户代码的单一可执行文件，
但遗憾的是这个导出的文件实际上是 SBCL 的 runtime 和 core 文件简单组合起来的产物，
因此非常之大，即使一行代码也不写这个文件也有 25MB 之多。

像 LispWorks 和 Allegro CL 这样的商业 Lisp 环境就提供了更强大的导出二进制程序的能力，
称为 delivery，基本原理是分析用户代码中可能用到的所有 Common Lisp 特性，
将不可能用到的部分从导出的 image 里直接清楚掉。
由于诸如 eval, compile-file, load , disassemble 这样的函数
其实也是 Common Lisp 环境的一部分，
而在正常的 Lisp 程序里这些特性，尤其是编译器和求值器等都是不会使用的，

所以这种办法可以有效地降低导出文件的尺寸。
最基本的 hello world 程序，LispWorks 的导出文件可以和用 gcc 编译出来的 C 实现一样精简。
而 Allegro CL 是没有导出独立可执行文件能力的，
它导出的可执行文件仍然需要 allegro runtime，一个动态连接库的支持，
并且每个二进制可执行文件需要配套一个 dxl 扩展名的文件存放具体的 Lisp image 数据。

SBCL 的 save-lisp-and-die 函数用法：

```lisp
Function: #<FUNCTION SAVE-LISP-AND-DIE>
Its associated name (as in FUNCTION-LAMBDA-EXPRESSION) is SAVE-LISP-AND-DIE.
The function's arguments are:  (CORE-FILE-NAME &KEY (TOPLEVEL #'TOPLEVEL-INIT)
                                (PURIFY NIL) (ROOT-STRUCTURES NIL)
                                (ENVIRONMENT-NAME auxiliary) (EXECUTABLE NIL))
```

其中对我们有帮助的主要参数有
* CORE-FILE-NAME: 最后导出的二进制文件名，
* EXECUTABLE: 设置导出为可执行文件。

下面首先做一个简单的例子。

打开 SBCL 环境，输入一个 hello 函数：
```lisp
(defun hello () (format t "hello, world!~%"))
```
然后导出并运行它：

```lisp
binghe@binghe-laptop:~$ sbcl --no-userinit
This is SBCL 1.0, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (defun hello () (format t "hello, world!~%"))

HELLO
* (save-lisp-and-die "hello" :executable t)
[undoing binding stack and other enclosing state... done]
[saving current Lisp image into /home/binghe/hello:
writing 1912 bytes from the read-only space at 0x01000000
writing 1936 bytes from the static space at 0x05000000
writing 25370624 bytes from the dynamic space at 0x09000000
done]
binghe@binghe-laptop:~$ ./hello --noinform --no-sysinit --no-userinit --noprint --disable-debugger --eval '(progn (hello) (quit))'
hello, world!
```

虽然命令行很繁琐，
但却可以保证不出错(而且你当然可以写一个wrapper脚本来代替手工输入大量固定参数)，
并且上述导出的 hello 可执行文件在任何没有安装 SBCL 的同体系 Linux 系统下都可以正常执行：
(一个长达 25MB 的只链接了基本 Linux 运行库并且内含整个 SBCL 的可执行文件)

```
binghe@binghe-laptop:~$ ldd hello
        linux-gate.so.1 =>  (0xffffe000)
        libdl.so.2 => /lib/tls/i686/cmov/libdl.so.2 (0xb7f30000)
        libpthread.so.0 => /lib/tls/i686/cmov/libpthread.so.0 (0xb7f1e000)
        libm.so.6 => /lib/tls/i686/cmov/libm.so.6 (0xb7ef8000)
        libc.so.6 => /lib/tls/i686/cmov/libc.so.6 (0xb7dc7000)
        /lib/ld-linux.so.2 (0xb7f51000)
binghe@binghe-laptop:~$ ls -l hello
-rwxr-xr-x 1 binghe staff 25550860 2006-12-03 21:10 hello
```

命令行参数可以通过 sb-ext:\*posix-argv\* 变量得到，和 C 的规则相同，
第一个命令行参数是运行的程序本身，
后面的就是用户输入的参数，但是 SBCL 程序自己的命令行参数不在此范围内。
对于刚才导出的 hello 文件来说，如果带 a b c 三个参数执行的话：

```lisp
binghe@binghe-laptop:~$ ./hello --noinform --no-sysinit --no-userinit a b c
* sb-ext:*posix-argv*

("./hello" "a" "b" "c")
```

此外还需要考虑更复杂的采用 asdf 加载的程序的处理。
其实没有什么特别的，
只需事先通过 clc:clc-require 或者 asdf:load-op 函数将所有代码加载到 Lisp 环境之中，
然后 save-lisp-and-die 成可执行程序，
最后只需选择适当的启动时执行函数即可。
SBCL 的文档还提及，所有加载的外部动态库，
导出 core 文件后重新加载时还会自动被加载回来，
因此不必担心这方面的问题。

如果假设 SBCL 环境已经存在，那么就仍然可以做出很方便的独立文件，却有效降低的文件尺寸。
例如一些只在本地使用的 Lisp 小程序，就可以采用下面的两种方式来处理：

第一种方法是对于单一的 Lisp 源代码文件的，只需用 compile-file 编译以后即可正常执行。
Debian 的 SBCL 安装依赖于一个称为 binfmt 内核特性：
通过识别非 elf 格式的二进制文件的文件头来调用其他程序来执行此文件，
而让用户觉得好像是操作系统本身在运行这个程序。
Debian 的 SBCL 环境提供了一个 sbcl-run 脚本来做这件事：

```bash
#!/bin/sh

# Wrapper script to run FASL or Lisp files quietly with Steel Bank
# Common Lisp. It is used by binfmt-support to execute SBCL fasls
# natively.
#
#  -- René van Bevern <rvb@pro-linux.de>, Sun Aug 28 15:18:41 2005

if [ "$1" ]; then
    program="$1"; shift
    sbcl --noinform --userinit /dev/null --disable-debugger --eval
    "(progn (load "$program ") (quit))" --end-toplevel-options "$@"

else
    echo "Usage: $0 sbcl-program [arguments ...]"
fi
```

可以看到，这个脚本完成了代替用户输入一大堆基本参数的智能。
这个脚本会被安装在 Linux 的 binfmt 子系统里。
当前系统里所有的 binfmt 可以在 /var/lib/binfmts 里看到：

```
binghe@binghe-laptop:/var/lib/binfmts$ ls -l
合計 20
-rw-r--r-- 1 root root 45 2006-12-01 23:40 cmucl
-rw-r--r-- 1 root root 56 2006-09-10 21:24 python2.3
-rw-r--r-- 1 root root 56 2006-09-10 21:24 python2.4
-rw-r--r-- 1 root root 49 2006-12-03 06:12 sbcl
-rw-r--r-- 1 root root 62 2006-09-22 00:32 utf8script
binghe@binghe-laptop:/var/lib/binfmts$ cat sbcl
sbcl
magic
0
# FASL x0a

/usr/lib/sbcl/sbcl-run
```

就是说，SBCL 编译的 fasl 文件有固定的文件头(# FASL x0a)，
如果操作系统发现某个可执行文件含有这个文件头，
就会调用 /usr/lib/sbcl/sbcl-run 来运行它，下面是示例代码：

```lisp
(sb-echo.lisp)

(in-package :cl-user)

(defun echo (args)
  (declare (type list args))
  (format t "~{~A ~}~%" args))

(eval-when (:load-toplevel)
  (echo (cdr *posix-argv*)))
```

上述代码直接用 compile-file 编译成 sb-echo.fasl
或者在 Emacs SLIME 里 c-c c-k 一下源代码以后只要加上可执行权限就可以直接使用了：

```
binghe@binghe-laptop:~/lisp/src$ ls -l sb-echo.fasl
-rwxr-xr-x 1 binghe staff 2607 2006-12-03 22:05 sb-echo.fasl
binghe@binghe-laptop:~/lisp/src$ ./sb-echo.fasl a b  c
a b c
binghe@binghe-laptop:~/lisp/src$ ./sb-echo.fasl

binghe@binghe-laptop:~/lisp/src$ echo a b  c
a b c
binghe@binghe-laptop:~/lisp/src$ echo
```

可以看到，这个程序跟 echo 命令的功能相似(不过其实还差得很远)。
需要本地有SBCL并且正确安装配置了 binfmt 才行，
带来的好处是二进制文件很小，这个例子里只有 2607 字节。

但是上述方法不适用于多个源代码文件的情况，
为了能处理多个源代码的本地加载运行，
SBCL 提供了 SB-EXECUTABLE 模块。
这个模块能做出一种 shell 脚本，脚本里内含有复杂的 sbcl 启动参数，
将脚本中其余部分的多个 fasl 文件本身加载到系统的 SBCL 环境中，然后再执行事先设置的启动函数。
(这个模块只能在 \*nix 环境下使用，win32 是不可能的了)

启动 SBCL 以后通过
```
(require :sb-executable)
```
即可加载 SB-EXECUTABLE。这个模块提供了一个函数 make-executable:

```
Function: #<FUNCTION SB-EXECUTABLE:MAKE-EXECUTABLE>
Its associated name (as in FUNCTION-LAMBDA-EXPRESSION) is
  SB-EXECUTABLE:MAKE-EXECUTABLE.
The function's arguments are:  (OUTPUT-FILE FASLS &KEY
                                (RUNTIME-FLAGS
                                 '(--disable-debugger --no-userinit
                                   --no-sysinit))
                                INITIAL-FUNCTION)
Function documentation:
  Write an executable called OUTPUT-FILE which can be run from the shell, by 'linking' together code from FASLS.  Actually works by concatenating them and prepending a #! header
Its defined argument types are:
  (T T &KEY (:RUNTIME-FLAGS T) (:INITIAL-FUNCTION T))
Its result type is:
  (VALUES (MEMBER NIL T) (SIGNED-BYTE 32) &OPTIONAL)
On Fri, Dec 1, 2006 05:37:43 PM [-8] it was compiled from:
SYS:CONTRIB;SB-EXECUTABLE;SB-EXECUTABLE.LISP.NEWEST
  Created: Friday, December 1, 2006 05:21:57 AM [-8]
```

使用这个模块就可以处理多个源代码文件的问题了，示例代码如下：

第一个文件 a.lisp:

```lisp
(in-package :cl-user)

(defun hello (args)
  (format t "hello:~{ ~A~}~%" args))
```

第二个文件 b.lisp:

```lisp
(in-package :cl-user)

(defun main ()
  (hello (car *posix-argv*)))
```

下面演示进入一个 SBCL 环境，先编译这两个文件，然后将这两个文件做在一个可执行脚本里的过程：

```lisp
binghe@binghe-laptop:~/lisp/src/sb-executable$ sbcl --no-userinit
This is SBCL 1.0, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
* (compile-file "a.lisp")

; compiling file "/home/binghe/lisp/src/sb-executable/a.lisp" (written 03 DEC 2006 10:24:07 PM):
; compiling (IN-PACKAGE :CL-USER)
; compiling (DEFUN HELLO ...)

; /home/binghe/lisp/src/sb-executable/a.fasl written
; compilation finished in 0:00:00
#P"/home/binghe/lisp/src/sb-executable/a.fasl"
NIL
NIL
* (compile-file "b.lisp")

; compiling file "/home/binghe/lisp/src/sb-executable/b.lisp" (written 03 DEC 2006 10:32:24 PM):
; compiling (IN-PACKAGE :CL-USER)
; compiling (DEFUN MAIN ...)

; /home/binghe/lisp/src/sb-executable/b.fasl written
; compilation finished in 0:00:00
#P"/home/binghe/lisp/src/sb-executable/b.fasl"
NIL
NIL
* (require :sb-executable)

("SB-EXECUTABLE")
* (sb-executable:make-executable "hello" '("a.fasl" "b.fasl") :initial-function 'main)

T
0
* (quit)
binghe@binghe-laptop:~/lisp/src/sb-executable$ ls -l hello
-rwxr-xr-x 1 binghe staff 4844 2006-12-03 22:35 hello
binghe@binghe-laptop:~/lisp/src/sb-executable$ ./hello a b  c
hello: a b c
```

其实上如果检查生成的 hello 文件的内容，就会发现 hello 其实是下面这个脚本片段和两个 fasl 文件接在一起的：

```bash
#!/bin/sh --
exec sbcl --noinform --disable-debugger --no-userinit --no-sysinit --eval "(with
-open-file (i "$0 " :element-type '(unsigned-byte 8)) (loop while (< ret 2) whe
n (= (read-byte i) 10) count 1 into ret) (load i) (funcall (quote MAIN)) (quit))
" --end-toplevel-options ${1+"$@"}
```

奇妙的 SB-EXECUTABLE！不过很可惜这个方法还是无法处理带有 asdf 软件包的情况，
如果你的程序依赖了任何 asdf 加载的软件包，就不能使用 SB-EXECUTABLE 了。
事实上 SBCL 下没有尽善尽美的解决方案，还是去买 LispWorks 吧，
在 LispWorks 下一切变得简单了。

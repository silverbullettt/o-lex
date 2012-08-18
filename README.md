# O-lex

  
## **O-lex是什么？**
o-lex (**O**riginal **LEX**xical parser)是一个由[DrRacket][0]编写，可用正则表达式描述词法规则的词法分析器。目前仅支持的一些基本的正则表达式模式，但足以描述词法规则。

[0]: http://racket-lang.org/ "DrRacket"
## **正则表达式语法**

#### **转义字符**
- **~**： 将下一个字符标记为一个特殊字符、或一个原义字符  

#### **通配符**
- **~d**：匹配一个数字字符
- **~c**：匹配一个英文字符
- **[xyz]**：字符集合，匹配括号中包含的任何一个字符，括号中的字符不需要转义
- **[^xyz]**：反字符集合，匹配不在括号中的任何一个字符，括号中的字符同样不需要转义

#### **或**
- **|**：表示多个模式中任意匹配一个，如“wh(at|ere)”，可匹配“what”或“where”

#### **数量限定**
- **+**：表示前面的字符（或模式）可出现一次或多次，如“Micro+soft”可匹配“Microsoft”和“Microoooosoft”
- __\*__：表示前面的字符（或模式）可出现零次或多次，如“~c*”可匹配空串和任意英文单词
- **?**：表现前面的字符（或模式）可出现零次或一次，如“(+|-)?~d+”可表示任意有符号整数和无符号整数

#### **特殊字符**
- 当‘~’、‘|’、‘[’、‘(’、‘+’、‘*’、‘?’这几个字符作为普通字符出现时，均要在前面加上转义字符‘~’


## **如何使用o-lex**
非常简单。  

`(make-lex-parser regex-list [cmt-tag] [str-tag])`，[lex-parser.rkt][lex]

- regex-list：表示词法规则的正则表达式以及对应token-type的列表
- cmt-tag：表示注释的符号，调用`make-lex-parser`时带上这个参数，则parser返回的单词流中不包含注释
- str-tag：表示字符串的符号，调用`make-lex-parser`时带上这个参数，则parser会去掉字符串开头和结尾的引号（否则单词的开头和结尾各有2个引号）

`make-lex-parser`接受词法规则，返回一个词法解析器，该解析器接受字符串，返回单词流。返回参数中包含单词类型、单词本身、所在行号以及在该行的偏移。

`(make-regex-recognizer regex)`，[regex-parser.rkt][regex]  
`(make-regex-matcher regex)`，[regex-parser.rkt][regex]

作为用正则表达式表示词法规则的副产品，o-lex还包含一个正则表达式的识别器和匹配器。前者可识别一个字符串是否符合给定的表达式；后者从一个字符串中找出符合表达式的所有串，并给出它们的起始位置。

[lex]: https://github.com/silverbullettt/o-lex/blob/master/lex-parser.rkt "lex-parser.rkt"
[regex]: https://github.com/silverbullettt/o-lex/blob/master/regex-parser.rkt "regex-parser.rkt"


## **示例**
1. 用`make-regex-recognizer`构造一个能识别一个字符串是否符合yyyy-MM-dd格式日期识别器
<pre><code>
\> (**define** data (make-regex-recognizer "~d~d~d~d-[01]~d-[0123]~d"))
\> (data "2012-08-18")
'accept
\> (data "2012.12.21")
'reject
</pre></code>

2. 用`make-regex-matcher	`提取出字符串中的数字
<pre><code>
\>(**define** num (make-regex-matcher "(~+|-)?~d+(.~d+)?(e(~+|-)?~d+(.~d+)?)?"))
\> (num "I'm 21 years old, I have $-123.45, Avogadro's constant equals 6.02e23.")
'(("21" . 4) ("-123.45" . 26) ("6.02e23" . 62))
</pre></code>

3. 用`make-lex-parser`构造词法分析器，并识别出代码中的词素
<pre><code>
\>(**define** lp (make-lex-parser '(("(\_|~c)(_|~c|~d)、*" id)
	                               ("~d+" num)
                                   ("\"[^\"]*\"" string)
                                   ("~+" plus)
                                   ("=" assign)
                                   ("~(" lbrac)
                                   (")" rbrac)
                                   (";" semi)
						           ("output" output))))
\>(lp (lp "x = 123+456;\noutput(x);")
'((id "x" 1 1)
      (assign "=" 1 3)
      (num "123" 1 5)
      (plus "+" 1 8)
      (num "456" 1 9)
      (semi ";" 1 12)
      (output "output" 2 1)
      (lbrac "(" 2 7)
      (id "x" 2 8)
      (rbrac ")" 2 9)
      (semi ";" 2 10))
</pre></code>



## **碎碎念**
你可以很明显的看到，o-lex有一些奇怪的地方，这完全是我的个人原因。比如为什么用‘~’而不用‘\’做转义字符呢，因为在 *Scheme* 里‘\’也是转义字符，这意味着如果我也用‘\’做转义字符，那么在正则表达式里像匹配一个普通的‘\’就得输入四个‘\’(比如`(define slash (make-regex-recognizer "\\\\")`），这太蛋疼了，所以我决定用‘~’，因为*Scheme*的`printf`中也用‘~’。

另外通配符也很少，没有‘.’，但你知道怎么写出匹配所有字符的表达式吗？没错，“[^]”就可以了……o-lex是词法分析器，支持正则表达式只是为了方便定义词法规则，编程语言的词法规则都很简单，所有很多功能我就懒得实现了- - o-lex对付源文件基本上完全够用了。

哦还有一个问题，`make-lex-parser` 的第一个参数是个列表，在这个列表中越靠后的词法规则优先级越高，所以关键字都必须放在id这种通配符规则的后面。这个bug影响不大，我也懒得解决了。

最后一个问题，不要跨行。有两种词素是可能跨行的，_块注释_ 和 _字符串_ ，o-lex还搞不定这两种词素，它们如果出现会导致后序词素行号出错。所以使用o-lex的时候就不要定义块注释，也不要整跨行字符串。目前o-lex不支持从文件中读入，不过这个问题很快就会解决……

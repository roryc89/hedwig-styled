module Html.Styled.Element where

import Hedwig (Trait)
import Html.Styled (Style, StyledHtml(..))

type StyledElement msg =  Array Style -> Array (Trait msg) -> Array (StyledHtml msg) -> StyledHtml msg

text :: forall msg. String -> StyledHtml msg
text = StyledText

--

a :: forall msg. StyledElement msg
a = StyledHtml "a"

abbr :: forall msg. StyledElement msg
abbr = StyledHtml "abbr"

address :: forall msg. StyledElement msg
address = StyledHtml "address"

article :: forall msg. StyledElement msg
article = StyledHtml "article"

aside :: forall msg. StyledElement msg
aside = StyledHtml "aside"

audio :: forall msg. StyledElement msg
audio = StyledHtml "audio"

b :: forall msg. StyledElement msg
b = StyledHtml "b"

bdi :: forall msg. StyledElement msg
bdi = StyledHtml "bdi"

bdo :: forall msg. StyledElement msg
bdo = StyledHtml "bdo"

blockquote :: forall msg. StyledElement msg
blockquote = StyledHtml "blockquote"

br :: forall msg. StyledElement msg
br = StyledHtml "br"

button :: forall msg. StyledElement msg
button = StyledHtml "button"

canvas :: forall msg. StyledElement msg
canvas = StyledHtml "canvas"

caption :: forall msg. StyledElement msg
caption = StyledHtml "caption"

cite :: forall msg. StyledElement msg
cite = StyledHtml "cite"

code :: forall msg. StyledElement msg
code = StyledHtml "code"

col :: forall msg. StyledElement msg
col = StyledHtml "col"

colgroup :: forall msg. StyledElement msg
colgroup = StyledHtml "colgroup"

datalist :: forall msg. StyledElement msg
datalist = StyledHtml "datalist"

dd :: forall msg. StyledElement msg
dd = StyledHtml "dd"

del :: forall msg. StyledElement msg
del = StyledHtml "del"

details :: forall msg. StyledElement msg
details = StyledHtml "details"

dfn :: forall msg. StyledElement msg
dfn = StyledHtml "dfn"

div :: forall msg. StyledElement msg
div = StyledHtml "div"

dl :: forall msg. StyledElement msg
dl = StyledHtml "dl"

dt :: forall msg. StyledElement msg
dt = StyledHtml "dt"

em :: forall msg. StyledElement msg
em = StyledHtml "em"

embed :: forall msg. StyledElement msg
embed = StyledHtml "embed"

fieldset :: forall msg. StyledElement msg
fieldset = StyledHtml "fieldset"

figcaption :: forall msg. StyledElement msg
figcaption = StyledHtml "figcaption"

figure :: forall msg. StyledElement msg
figure = StyledHtml "figure"

footer :: forall msg. StyledElement msg
footer = StyledHtml "footer"

form :: forall msg. StyledElement msg
form = StyledHtml "form"

h1 :: forall msg. StyledElement msg
h1 = StyledHtml "h1"

h2 :: forall msg. StyledElement msg
h2 = StyledHtml "h2"

h3 :: forall msg. StyledElement msg
h3 = StyledHtml "h3"

h4 :: forall msg. StyledElement msg
h4 = StyledHtml "h4"

h5 :: forall msg. StyledElement msg
h5 = StyledHtml "h5"

h6 :: forall msg. StyledElement msg
h6 = StyledHtml "h6"

header :: forall msg. StyledElement msg
header = StyledHtml "header"

hr :: forall msg. StyledElement msg
hr = StyledHtml "hr"

i :: forall msg. StyledElement msg
i = StyledHtml "i"

iframe :: forall msg. StyledElement msg
iframe = StyledHtml "iframe"

img :: forall msg. StyledElement msg
img = StyledHtml "img"

input :: forall msg. StyledElement msg
input = StyledHtml "input"

ins :: forall msg. StyledElement msg
ins = StyledHtml "ins"

kbd :: forall msg. StyledElement msg
kbd = StyledHtml "kbd"

label :: forall msg. StyledElement msg
label = StyledHtml "label"

legend :: forall msg. StyledElement msg
legend = StyledHtml "legend"

li :: forall msg. StyledElement msg
li = StyledHtml "li"

main :: forall msg. StyledElement msg
main = StyledHtml "main"

map :: forall msg. StyledElement msg
map = StyledHtml "map"

mark :: forall msg. StyledElement msg
mark = StyledHtml "mark"

math :: forall msg. StyledElement msg
math = StyledHtml "math"

menu :: forall msg. StyledElement msg
menu = StyledHtml "menu"

menuitem :: forall msg. StyledElement msg
menuitem = StyledHtml "menuitem"

meter :: forall msg. StyledElement msg
meter = StyledHtml "meter"

nav :: forall msg. StyledElement msg
nav = StyledHtml "nav"

node :: forall msg. StyledElement msg
node = StyledHtml "node"

object :: forall msg. StyledElement msg
object = StyledHtml "object"

ol :: forall msg. StyledElement msg
ol = StyledHtml "ol"

optgroup :: forall msg. StyledElement msg
optgroup = StyledHtml "optgroup"

option :: forall msg. StyledElement msg
option = StyledHtml "option"

output :: forall msg. StyledElement msg
output = StyledHtml "output"

p :: forall msg. StyledElement msg
p = StyledHtml "p"

param :: forall msg. StyledElement msg
param = StyledHtml "param"

pre :: forall msg. StyledElement msg
pre = StyledHtml "pre"

progress :: forall msg. StyledElement msg
progress = StyledHtml "progress"

q :: forall msg. StyledElement msg
q = StyledHtml "q"

rp :: forall msg. StyledElement msg
rp = StyledHtml "rp"

rt :: forall msg. StyledElement msg
rt = StyledHtml "rt"

ruby :: forall msg. StyledElement msg
ruby = StyledHtml "ruby"

s :: forall msg. StyledElement msg
s = StyledHtml "s"

samp :: forall msg. StyledElement msg
samp = StyledHtml "samp"

section :: forall msg. StyledElement msg
section = StyledHtml "section"

select :: forall msg. StyledElement msg
select = StyledHtml "select"

small :: forall msg. StyledElement msg
small = StyledHtml "small"

source :: forall msg. StyledElement msg
source = StyledHtml "source"

span :: forall msg. StyledElement msg
span = StyledHtml "span"

strong :: forall msg. StyledElement msg
strong = StyledHtml "strong"

sub :: forall msg. StyledElement msg
sub = StyledHtml "sub"

summary :: forall msg. StyledElement msg
summary = StyledHtml "summary"

sup :: forall msg. StyledElement msg
sup = StyledHtml "sup"

table :: forall msg. StyledElement msg
table = StyledHtml "table"

tbody :: forall msg. StyledElement msg
tbody = StyledHtml "tbody"

td :: forall msg. StyledElement msg
td = StyledHtml "td"

textarea :: forall msg. StyledElement msg
textarea = StyledHtml "textarea"

tfoot :: forall msg. StyledElement msg
tfoot = StyledHtml "tfoot"

th :: forall msg. StyledElement msg
th = StyledHtml "th"

thead :: forall msg. StyledElement msg
thead = StyledHtml "thead"

time :: forall msg. StyledElement msg
time = StyledHtml "time"

tr :: forall msg. StyledElement msg
tr = StyledHtml "tr"

track :: forall msg. StyledElement msg
track = StyledHtml "track"

u :: forall msg. StyledElement msg
u = StyledHtml "u"

ul :: forall msg. StyledElement msg
ul = StyledHtml "ul"

var :: forall msg. StyledElement msg
var = StyledHtml "var"

video :: forall msg. StyledElement msg
video = StyledHtml "video"

wbr :: forall msg. StyledElement msg
wbr = StyledHtml "wbr"

# Keycodes

Keys with modifiers are represented as combinations such as `"ctrl-p"`
or `"alt-f"` or `"ctrl-alt-x"`. The shift key is not taken into
account when determining key bindings, but it can be checked inside
your function using `helm.isDown("lshift", "rshift")`.

## Character keys

a 	The A key
b 	The B key
c 	The C key
d 	The D key
e 	The E key
f 	The F key
g 	The G key
h 	The H key
i 	The I key
j 	The J key
k 	The K key
l 	The L key
m 	The M key
n 	The N key
o 	The O key
p 	The P key
q 	The Q key
r 	The R key
s 	The S key
t 	The T key
u 	The U key
v 	The V key
w 	The W key
x 	The X key
y 	The Y key
z 	The Z key
0 	The zero key
1 	The one key
2 	The two key
3 	The three key
4 	The four key
5 	The five key
6 	The six key
7 	The seven key
8 	The eight key
9 	The nine key
  	Space key (use a " " string)
! 	Exclamation mark key
" 	Double quote key
\# 	Hash key
$ 	Dollar key
& 	Ampersand key
' 	Single quote key
( 	Left parenthesis key
) 	Right parenthesis key
* 	Asterisk key
+ 	Plus key
, 	Comma key
- 	Hyphen-minus key
. 	Full stop key
/ 	Slash key
: 	Colon key
; 	Semicolon key
< 	Less-than key
= 	Equal key
> 	Greater-than key
? 	Question mark key
@ 	At sign key
[ 	Left square bracket key
\ 	Backslash key
] 	Right square bracket key
^ 	Caret key
_ 	Underscore key
` 	Grave accent key 	Also known as the "Back tick" key

## Numpad keys

kp0 	The numpad zero key
kp1 	The numpad one key
kp2 	The numpad two key
kp3 	The numpad three key
kp4 	The numpad four key
kp5 	The numpad five key
kp6 	The numpad six key
kp7 	The numpad seven key
kp8 	The numpad eight key
kp9 	The numpad nine key
kp. 	The numpad decimal point key
kp, 	The numpad comma key
kp/ 	The numpad division key
kp* 	The numpad multiplication key
kp- 	The numpad substraction key
kp+ 	The numpad addition key
kpenter 	The numpad enter key
kp= 	The numpad equals key

## Navigation keys

up 	Up arrow key
down 	Down arrow key
right 	Right arrow key
left 	Left arrow key
home 	Home key
end 	End key
pageup 	Page up key
pagedown 	Page down key

## Editing keys

insert 	Insert key
backspace 	Backspace key
tab 	Tab key
clear 	Clear key
return 	Return key 	Also known as the Enter key
delete 	Delete key

## Function keys

f1 	The 1st function key
f2 	The 2nd function key
f3 	The 3rd function key
f4 	The 4th function key
f5 	The 5th function key
f6 	The 6th function key
f7 	The 7th function key
f8 	The 8th function key
f9 	The 9th function key
f10 	The 10th function key
f11 	The 11th function key
f12 	The 12th function key
f13 	The 13th function key
f14 	The 14th function key
f15 	The 15th function key
f16 	The 16th function key
f17 	The 17th function key
f18 	The 18th function key

## Modifier keys

numlock 	Num-lock key 	Clear on Mac keyboards.
capslock 	Caps-lock key 	Caps-on is a key press. Caps-off is a key release.
scrolllock 	Scroll-lock key
rshift 	Right shift key
lshift 	Left shift key
rctrl 	Right control key
lctrl 	Left control key
ralt 	Right alt key
lalt 	Left alt key
rgui 	Right gui key 	Command key in OS X, Windows key in Windows.
lgui 	Left gui key 	Command key in OS X, Windows key in Windows.
mode 	Mode key

## Application keys

www 	WWW key
mail 	Mail key
calculator 	Calculator key
computer 	Computer key
appsearch 	Application search key
apphome 	Application home key
appback 	Application back key
appforward 	Application forward key
apprefresh 	Application refresh key
appbookmarks 	Application bookmarks key

## Miscellaneous keys

pause 	Pause key 	Sends a key release immediately on some platforms, even if held down.
escape 	Escape key
help 	Help key
printscreen 	Printscreen key 	Sends a key release immediately on Windows, even if held down.
sysreq 	System request key
menu 	Menu key
application 	Application key 	Windows contextual menu, compose key.
power 	Power key
currencyunit 	Currency unit key 	e.g. the Euro (€) key.
undo 	Undo key

## Mouse buttons

wheelup	Mousewheel Up
wheeldown	Mousewheel Down
wheelleft	Mousewheel Left
wheelright	Mousewheel Right

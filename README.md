# Clarion-Hint-library
Библиотека подсказок

# Dec2Hex Hex2Dec преобразования

!----------------------------------------------------------------------

    DEC2HEX(LONG PARAM:LNUMBER),STRING,PRIVATE
    HEX2DEC(STRING PARAM:STNUMBER),LONG,PRIVATE
	
!----------------------------------------------------------------------
```
Dec2Hex PROCEDURE(LONG Param:lNumber)

Loc:stBuffer         STRING(20)
Loc:lNumber          LONG
Loc:lRemainder       LONG
Loc:lLenBuffer       LONG
Loc:stSign           String(' ')

  CODE
  CLEAR(Loc:stBuffer)
  Loc:lNumber = Param:lNumber
  IF Loc:lNumber < 0
     Loc:lNumber = -Loc:lNumber
     Loc:stSign = '-'
  END

  LOOP
    Loc:lRemainder = Loc:lNumber % 16
    Loc:lNumber = ( Loc:lNumber - Loc:lRemainder ) / 16
    IF Loc:lRemainder > 9
      Loc:stBuffer = CHR(Loc:lRemainder - 10 + VAL('A')) &
CLIP(Loc:stBuffer)
    ELSE
      Loc:stBuffer = CHR(Loc:lRemainder + VAL('0')) & CLIP(Loc:stBuffer)
    END
    IF ~Loc:lNumber THEN BREAK.
  END
  Loc:lLenBuffer = LEN(CLIP(Loc:stBuffer))
  IF Loc:lLenBuffer < 6
     Loc:stBuffer = ALL('0',6 - Loc:lLenBuffer) & CLIP(Loc:stBuffer)
  END
  RETURN(CLIP(Loc:stSign) & CLIP(Loc:stBuffer))
!----------------------------------------------------------------------

```
```
Hex2Dec PROCEDURE(STRING Param:stNumber)

Loc:lLoopIndex  LONG
Loc:lSizeString LONG
Loc:lNumber     LONG(0)
Loc:lRemainder  LONG
Loc:lSign       LONG(1)

  CODE
  Param:stNumber = CLIP(LEFT(Param:stNumber))
  Loc:lSizeString = LEN(CLIP(Param:stNumber))
  IF Param:stNumber[1] = '-'
    Param:stNumber = SUB(Param:stNumber,2,LEN(CLIP(Param:stNumber)) - 1)
    Loc:lSign = -1
  END
  LOOP Loc:lLoopIndex = 1 TO Loc:lSizeString
    IF VAL(Param:stNumber[Loc:lSizeString - (Loc:lLoopIndex - 1)]) >
VAL('9')
      Loc:lRemainder = 10 + VAL(Param:stNumber[Loc:lSizeString -
(Loc:lLoopIndex - 1)]) - VAL('A')
    ELSE
      Loc:lRemainder = VAL(Param:stNumber[Loc:lSizeString -
(Loc:lLoopIndex - 1)]) - VAL('0')
    END
    Loc:lNumber += 16 ^ (Loc:lLoopIndex - 1) * Loc:lRemainder
  END
  RETURN(Loc:lSign * Loc:lNumber)
```
!----------------------------------------------------------------------

# А можно без InString?

Гарантирую - будет быстрее работать.

```
RInString            FUNCTION(STRING _pSub,*STRING _pString,<UNSIGNED _pStep>,<UNSIGNED _pStart>)

lSub                 LONG,AUTO
lStr                 LONG,AUTO
Pos                  LONG,AUTO

  Code
  lSub = Len(_pSub); lStr = Len(_pString)
  if ~lSub OR (lStr < lSub) then Return(0).
  if Omitted(3) then _pStep = lSub.
  if Omitted(4) OR (_pStart > lStr) then _pStart = lStr.

  Pos = _pStart - lSub + 1
  Loop While Pos
    if _pString[Pos] = _pSub[1]
       if _pString[Pos : (Pos + lSub - 1)] = _pSub then Return(Pos).
    .
    Pos -= _pStep
  .
  Return(0)
```

Второй параметр передается по адресу для ускорения
обработки - в этом случае при передаче параметров
в процедуру не используется строковый стек.
Если опущен последний параметр, то поиск начинается
с конца строки _pString.
Вообщем, синтаксис - практически аналогичный стандартной InString.

# Авто переключение RU LAT

Точки вставки Слара-5 для авто переключения русского и латинского ввода в поля
!авто переключатель RUS LAT

****************after global includes
```
  include('winequ.clw')
```


****************before global includes
```
Latin    CSTRING('00000409')
Russian  CSTRING('00000419')
```


****************inside the global map
```
SwitchKBD(String KBDLayout)
MODULE('Windows.lib')
  GetKeyboardLayoutList(SIGNED,*HKL),SIGNED,PASCAL,RAW
  ActivateKeyboardLayout(HKL,UNSIGNED),HKL,PASCAL

  GetKeyboardLayoutName(*LPSTR),BOOL,PASCAL,RAW,NAME('GetKeyboarfLayoutName')
  GetKeyboardLayout(DWORD),HKL,PASCAL
  GetKeyboardLayoutNameA(*LPSTR),BOOL,PASCAL,RAW
end
```


*****************global     programm procedures
```
SwitchKBD Procedure (KBDLayout)
AKL  HKL
TMP  CSTRING(20)
loc:i Long
 Code
 loc:i = GetKeyboardLayoutNameA(Tmp)
 if Tmp<>KBDLayout
 AKL = ActivateKeyBoardLayout(1,0)
end
```





****************** в поле enter --->?selected
SwitchKBD(Russian)           !русский режим клавиатуры

****************** в поле enter --->?selected
SwitchKBD(Latin)            !латинский режим клавиатуры




```
SwitchKBD            PROCEDURE  (KBDLayout) !(*CSTRING)               ! Declare Procedure
AKL  HKL
TMP  CSTRING(20)
loc:i Long
  CODE
 loc:i = GetKeyboardLayoutNameA(Tmp)
 IF Tmp<>KBDLayout THEN AKL = ActivateKeyBoardLayout(1,0).
```



!----------------------------------------------------------------------

!----------------------------------------------------------------------

# h1 заголовок первого уровня

!----------------------------------------------------------------------

!----------------------------------------------------------------------

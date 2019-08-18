# Clarion-Hint-library
Библиотека подсказок

# Dec2Hex Hex2Dec преобразования

!------------------------------------------------------

    DEC2HEX(LONG PARAM:LNUMBER),STRING,PRIVATE
    HEX2DEC(STRING PARAM:STNUMBER),LONG,PRIVATE
    
!------------------------------------------------------
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

# h1 заголовок первого уровня

!----------------------------------------------------------------------

!----------------------------------------------------------------------

# h1 заголовок первого уровня

!----------------------------------------------------------------------

!----------------------------------------------------------------------

# h1 заголовок первого уровня

!----------------------------------------------------------------------

!----------------------------------------------------------------------

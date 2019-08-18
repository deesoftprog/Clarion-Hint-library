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

# Вычисление размера файла

```
GetFileInfo     PROCEDURE(String PassedFileName,String PassedType)
LocalFileQueue  Queue(File:queue),Pre(FIL)
                 End
   CODE

   Directory(LocalFileQueue,PassedFileName,ff_:NORMAL)

   If Records(LocalFileQueue) = 1
     Get(LocalFileQueue,1)
     If Error()
       Message(Error())
     Else
       Free(LocalFileQueue)

       Case PassedType
       Of 1
         Return LocalFileQueue.Size
       Of 2
         Return LocalFileQueue.Date
       Of 3
         Return LocalFileQueue.Time
       Else
         Return FALSE
       End
     End
   End

   Free(LocalFileQueue)

   Return FALSE
```

!----------------------------------------------------------------------

!----------------------------------------------------------------------

# Вычисление CRC

 ```
 MAP
    Calc_CRC(ULONG _StartAddr,ULONG _EndAddr),ULONG
  END

Calc_CRC PROCEDURE(ULONG _StartAddr,ULONG _EndAddr)

loc:CRC       ULONG,AUTO
loc:Bit_Count BYTE,AUTO
loc:Char_Ptr  ULONG,AUTO
loc:Char      BYTE,AUTO

  Code
  CLEAR(loc:CRC,1)
  LOOP loc:Char_Ptr = _StartAddr to _EndAddr
    PEEK(loc:Char_Ptr,loc:Char)
    loc:CRC = BXOR(loc:CRC,loc:Char)
    LOOP loc:Bit_Count = 0 to 7
      IF BAND(loc:CRC,1)
         loc:CRC = BXOR(BSHIFT(loc:CRC,-1),0A001h)
      ELSE
         loc:CRC = BSHIFT(loc:CRC,-1)
  . . .
  Return(loc:CRC)
```


# Перевод Long to Hex

```
Long2Hex    Procedure(Long X)
Loc:X       Long
Loc:Digit   Short
Loc:Hex     String('00000000')
            Code
            Loc:X=X
            Loop i#=8 To 1 By -1
              Loc:Digit=Loc:X % 16
              Loc:Hex[I#]=Sub('0123456789ABCDEF',Loc:Digit+1,1)
              Loc:X = (Loc:X-Loc:Digit) / 16
              If Loc:X=0 Then Break.
            End
            Return(Loc:Hex)
```

# Транслитерация

```
ChangeName Function(RusName)
EngName     String(255)
            Code
            J = 1
            Loop I = 1 To Len(Clip(RusName))
                Case RusName[I]
                    OF 'я'
                        EngName[J] = 'j'
                        J += 1
                        EngName[J] = 'a'
                    OF 'Я'
                        EngName[J] = 'J'
                        J += 1
                        EngName[J] = 'a'
                    OF 'с'
                        EngName[J] = 's'
                    OF 'С'
                        EngName[J] = 'S'
                    OF 'ч'
                        EngName[J] = 't'
                        J += 1
                        EngName[J] = 's'
                        J += 1
                        EngName[J] = 'c'
                        J += 1
                        EngName[J] = 'h'
                    OF 'Ч'
                        EngName[J] = 'T'
                        J += 1
                        EngName[J] = 's'
                        J += 1
                        EngName[J] = 'c'
                        J += 1
                        EngName[J] = 'h'
                    OF 'м'
                        EngName[J] = 'M'
                    OF 'М'
                        EngName[J] = 'M'
                    OF 'и' OROF 'й'
                        EngName[J] = 'i'
                    OF 'И' OROF 'Й'
                        EngName[J] = 'I'
                    OF 'т'
                        EngName[J] = 't'
                    OF 'Т'
                        EngName[J] = 'T'
                    OF 'б'
                        EngName[J] = 'b'
                    OF 'Б'
                        EngName[J] = 'B'
                    OF 'ю'
                        EngName[J] = 'j'
                        J += 1
                        EngName[J] = 'u'
                    OF 'Ю'
                        EngName[J] = 'J'
                        J += 1
                        EngName[J] = 'u'
                    OF 'ц'
                        EngName[J] = 't'
                        J += 1
                        EngName[J] = 's'
                    OF 'Ц'
                        EngName[J] = 'T'
                        J += 1
                        EngName[J] = 's'
                    OF 'у'
                        EngName[J] = 'u'
                    OF 'У'
                        EngName[J] = 'U'
                    OF 'к'
                        EngName[J] = 'k'
                    OF 'К'
                        EngName[J] = 'K'
                    OF 'е' OROF 'ё'
                        EngName[J] = 'e'
                    OF 'Е' OROF 'Ё'
                        EngName[J] = 'E'
                    OF 'н'
                        EngName[J] = 'n'
                    OF 'Н'
                        EngName[J] = 'N'
                    OF 'г'
                        EngName[J] = 'g'
                    OF 'Г'
                        EngName[J] = 'G'
                    OF 'ш' OROF 'щ'
                        EngName[J] = 's'
                        J += 1
                        EngName[J] = 'h'
                    OF 'Ш' OROF 'Щ'
                        EngName[J] = 'S'
                        J += 1
                        EngName[J] = 'h'
                    OF 'з'
                        EngName[J] = 'z'
                    OF 'З'
                        EngName[J] = 'Z'
                    OF 'х'
                        EngName[J] = 'c'
                        J += 1
                        EngName[J] = 'h'
                    OF 'Х'
                        EngName[J] = 'C'
                        J += 1
                        EngName[J] = 'h'
                    OF 'ф'
                        EngName[J] = 'f'
                    OF 'Ф'
                        EngName[J] = 'F'
                    OF 'в'
                        EngName[J] = 'v'
                    OF 'В'
                        EngName[J] = 'V'
                    OF 'а'
                        EngName[J] = 'a'
                    OF 'А'
                        EngName[J] = 'A'
                    OF 'п'
                        EngName[J] = 'p'
                    OF 'П'
                        EngName[J] = 'P'
                    OF 'р'
                        EngName[J] = 'r'
                    OF 'Р'
                        EngName[J] = 'R'
                    OF 'о'
                        EngName[J] = 'o'
                    OF 'О'
                        EngName[J] = 'O'
                    OF 'л'
                        EngName[J] = 'l'
                    OF 'Л'
                        EngName[J] = 'L'
                    OF 'д'
                        EngName[J] = 'd'
                    OF 'Д'
                        EngName[J] = 'D'
                    OF 'ж'
                        EngName[J] = 'z'
                        J += 1
                        EngName[J] = 'h'
                    OF 'Ж'
                        EngName[J] = 'Z'
                        J += 1
                        EngName[J] = 'h'
                    OF 'э'
                        EngName[J] = 'e'
                    OF 'Э'
                        EngName[J] = 'E'
                    OF 'ь' OROF 'ъ'
                        J -= 1
                    OF 'Ь' OROF 'Ъ'
                        J -= 1
                    Else
                        EngName[J] = RusName[I]
                End
                J += 1
            End
            Return(Clip(EngName))
```
			
# Уникальный индификатор

```
! xGenGlobalID         PROCEDURE(), STRING      получить уникальный индификатор записи
!--------------------------------------------------------------------------------
xGenGlobalID           PROCEDURE()
GlobalID               STRING(20)
  CODE
  LOOP i# = 1 to 20
    EXECUTE RANDOM(1,3)
      GlobalID[i#] = CHR(RANDOM(48,57))
      GlobalID[i#] = CHR(RANDOM(65,90))
      GlobalID[i#] = CHR(RANDOM(97,122))
    END
  END
  RETURN GlobalID
```
# Функция замены в строке

```
Replace PROCEDURE(STRING Text,STRING Find,STRING Replace,BYTE CS=FALSE)
Pos  UNSIGNED,AUTO
RVal ANY ! Do not add ,AUTO
  CODE
    LOOP
      Pos =  CHOOSE(~CS,INSTRING(UPPER(Find),UPPER(Text),1),INSTRING(Find,Text,1))
      RVal = RVal & CHOOSE(~Pos,Text,Text[1 : Pos-1] & Replace)
      Text = CHOOSE(~Pos,'',Text[Pos+LEN(Find) : LEN(Text)])
    UNTIL ~Text
    RETURN RVal
```

# Функция разрезания строки на куски

```
!------------------------------------------------------------------------------!
! Функция разбивающая строку на равные части без "разрезания" слов.            !
!------------------------------------------------------------------------------!
AlignString  PROCEDURE( DestStr, SourceStr, CharPerLine )

SourceStr    EXTERNAL                            ! Исходная строка
DestStr      EXTERNAL                            ! Строка результат
CharPerLine  SHORT                               ! Длина одной линии

LEN_SOURCE   LONG                                ! Длина исходной строки
EOS          SHORT                               ! Признак конца строки
NDX          LONG                                ! Индекс текущего символа

LEN_DEST     LONG                                ! Длина результата

CUR_LINE     STRING( 255 )                       ! Текущая линия
LEN_LINE     SHORT                               ! Длина текущей линии

INWORD       SHORT                               ! Флаг "внутри слова"
LEN_WORD     SHORT                               ! Длина текущего слова
CHAR_EMPTY   SHORT                               ! Признак пустого символа

  CODE

  LEN_SOURCE = LEN( CLIP( SourceStr ) )          ! Вычислить длину исходной стр.
  EOS        = FALSE                             ! Строка не закончилась
  NDX      = 1                                   ! Текущий - первый символ

  CLEAR( DestStr )                               ! Очистить выходную строку
  LEN_DEST = 0                                   ! Обнулить ее длину

  CLEAR( CUR_LINE )                              ! Очистить текущую линию
  LEN_LINE = 0                                   ! Обнулить ее длину

  INWORD   = FALSE                               ! Флаг - "вне слова"
  LEN_WORD = 0                                   ! Обнулить длину слова

  DO GET_CHAR
  LOOP WHILE NOT EOS
    IF CHAR_EMPTY
      IF INWORD
        INWORD = FALSE
        IF LEN_LINE + LEN_WORD > CharPerLine
          DestStr = SUB( DestStr, 1, LEN_DEST ) & CUR_LINE
          CLEAR( CUR_LINE )
          LEN_LINE = 0
          LEN_DEST += CharPerLine
        .
        CUR_LINE = SUB( CUR_LINE, 1, LEN_LINE ) |
                   & SUB( SourceStr, NDX - LEN_WORD - 1, LEN_WORD )
        LEN_LINE += LEN_WORD + 1
        LEN_WORD = 0
      .
    ELSE
      IF INWORD
        LEN_WORD += 1
      ELSE
        LEN_WORD = 1
        INWORD = TRUE
    . .
    DO GET_CHAR
  .
  DestStr = SUB( DestStr, 1, LEN_DEST ) & CUR_LINE

!------------ Получить очередной символ из строки ------------------------------

GET_CHAR     ROUTINE

  IF NDX > LEN_SOURCE                            ! Если строка закончилась
    IF NDX = LEN_SOURCE + 1                      !   Если только что
      CHAR_EMPTY = TRUE                          !     то эмулируем пустой
    ELSE                                         !   иначе
      EOS = TRUE                                 !     конец строки
    .                                            ! иначе
  ELSE                                           !   Проверим текущий
    CASE VAL( SUB( SourceStr, NDX, 1 ) )         !   символ на пустоту
    OF 9 OROF 10 OROF 13 OROF 32                 !
      CHAR_EMPTY = TRUE                          !
    ELSE                                         !
      CHAR_EMPTY = FALSE                         !
  . .                                            !
  NDX += 1                                       ! Переход к следующему символу

```

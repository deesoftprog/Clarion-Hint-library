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

# перевод регистра

```
!Эта процедура корректно переводит строку Str, содержащую русские буквы в верхний регистр.
!Строка передается по ссылке, то есть прототип ее (*String).   Ничего не возвращает, но изменяет оригинал строки Str.

!UpperStr PROCEDURE ( Str )  ! Перевод строки с русскими буквами в верхний регистр

CODE
  x# = len( Str )
  loop i#=1 to x#
     ch# = val( sub( Str, i#, 1 ) )
     if ch# >= val('а') and ch# <= val('я')
        Str[ i# ] = chr( ch#-32 )
     else
        Str[ i# ] = upper( chr(ch#) )
     .
  .


!LowerStr
!Эта процедура корректно переводит строку Str, содержащую русские буквы в нижний регистр.
!Строка передается по ссылке, то есть прототип ее (*String).  Ничего не возвращает, но изменяет оригинал строки Str.

LowerStr PROCEDURE ( Str )  ! Перевод строки с русскими буквами в верхний регистр
CODE
  x# = len( Str )
  loop i#=1 to x#
     ch# = val( sub( Str, i#, 1 ) )
     if ch# >= val('А') and ch# <= val('Я')
        Str[ i# ] = chr( ch#+32 )
     else
        Str[ i# ] = lower( chr(ch#) )
     .
  .

```

# Сумма прописью

```
Функция SummProp
Это очередное решение популярной задачи печати суммы прописью в бухгалтерских документах.   Каждый программист, работающий в этой области, наверняка её решал.  Я лет десять назад написал её сначала на Basic-е, а потом перевел и на Clarion.

SummProp( Summ )     Перевод числа в сумму прописью

Summ - Исходное число (REAL).   Обрабатываются только положительные числа от нуля до 999 миллиардов.  Дробная часть отбрасывается (то есть копейки и центы выделяются и печатаются не в этой функции).

Возвращает строку

Прототип: SummProp( Real ), String

SummProp FUNCTION (Summ)
!============================================

MLR      REAL        ! Миллиарды
MIL      LONG        ! Миллионы
TYS      LONG        ! Тысячи
EDI      LONG        ! Единицы
FIRST    LONG        ! Первая буква строки
Str      STRING(250)
SummProp STRING(255) ! Строка для результата

CODE
Summ = abs( Summ )
if Summ < 1
   return( 'Ноль ' )
.

! --- Миллиарды ---
SummProp = ''
MLR = int( Summ/1000000000.0)
if MLR > 0 and MLR < 1000
   SummProp = I_TO_STR( MLR )     ! Функция перевода трехзначного числа в строку
   if MLR = 1
     SummProp = CLIP(SummProp) & ' миллиард '
   elsif MLR=2 or MLR=3 or MLR=4
     SummProp = CLIP(SummProp) & ' миллиарда '
   else
     SummProp = CLIP(SummProp) & ' миллиардов '
   .
.

! --- Миллионы ---
MIL = int( (Summ - MLR*1000000000) / 1000000.0)
if MIL <> 0
   if SummProp<>''
     SummProp = clip(SummProp) &' '& I_TO_STR( MIL )
   else
     SummProp = I_TO_STR( MIL )
   .
   STR = sub( clip(SummProp), -7, 7 )
   if instring( 'один', STR, 1 ) THEN
     SummProp = CLIP(SummProp) & ' миллион '
   elsif ( instring( 'два', STR, 1 ) or |
           instring( 'три', STR, 1 ) or |
           instring( 'четыре', STR, 1 ) )
     SummProp = CLIP(SummProp) & ' миллиона '
   else
     SummProp = CLIP(SummProp) & ' миллионов '
   .
.

! --- Тысячи ---
TYS = int( (Summ - MLR*1000000000.0 - MIL*1000000.0)/1000.0 )
if TYS <> 0
   if SummProp<>''
     SummProp = clip(SummProp) &' '& I_TO_STR( TYS )
   else
     SummProp = I_TO_STR( TYS )
   .
   STR = sub( CLIP(SummProp), -7, 7 )
   if instring( 'один', STR, 1 )
     STR = sub( clip(SummProp), 1, len( clip(SummProp) )-2 )
     SummProp = CLIP(STR) & 'на тысяча'
   elsif instring( 'два', STR, 1 )
     STR = sub( clip(SummProp), 1, len( clip(SummProp) )-1 )
     SummProp = clip(STR) & 'е тысячи'
   elsif instring( 'три',STR,1 ) or instring( 'четыре',STR,1 )
     SummProp = clip(SummProp) & ' тысячи'
   else
     SummProp = clip(SummProp) & ' тысяч'
   .
.

! -- Cотни, десятки и единицы --
EDI = Summ - MLR*1000000000.0 - MIL*1000000.0 - TYS*1000.0
if SummProp<>''
   SummProp = clip(SummProp) &' '& I_TO_STR( EDI )
else
   SummProp = I_TO_STR( EDI )
.

! --- Сделать первую букву заглавной ---
STR = sub( SummProp, 2, len( clip(SummProp) )-1 )
FIRST = val( sub( SummProp, 1, 1 ) )
SummProp = chr( R_UPPER(FIRST) ) & clip(STR)  ! Перевод символа в верхний регистр
return( SummProp )


I_TO_STR  FUNCTION (NUM)  ! Перевод трехзначного числа в строку пописью
!============================================

HUN      LONG  ! Сотни
DEC      LONG  ! Десятки
UNI      LONG  ! Единицы
SummProp STRING(255)

CODE

HUN = int( NUM/100 )            ! Сотни
DEC = int( (NUM - HUN*100)/10 ) ! Десятки
UNI = NUM - HUN*100 - DEC*10    ! Единицы
! -- Сотни --
if HUN > 0 or HUN < 10
   case HUN
   of 1 ; SummProp = clip(SummProp) & ' сто'
   of 2 ; SummProp = clip(SummProp) & ' двести'
   of 3 ; SummProp = CLIP(SummProp) & ' триста'
   of 4 ; SummProp = CLIP(SummProp) & ' четыреста'
   of 5 ; SummProp = CLIP(SummProp) & ' пятьсот'
   of 6 ; SummProp = CLIP(SummProp) & ' шестьсот'
   of 7 ; SummProp = CLIP(SummProp) & ' семьсот'
   of 8 ; SummProp = CLIP(SummProp) & ' восемьсот'
   of 9 ; SummProp = CLIP(SummProp) & ' девятьсот'
   .
.
! -- Десятки --
if DEC <> 0
   case DEC
   of 1
     case UNI
     of 0 ; SummProp = clip(SummProp) & ' десять'
     OF 1 ; SummProp = CLIP(SummProp) & ' одиннадцать'
     OF 2 ; SummProp = CLIP(SummProp) & ' двенадцать'
     OF 3 ; SummProp = CLIP(SummProp) & ' тринадцать'
     OF 4 ; SummProp = CLIP(SummProp) & ' четырнадцать'
     OF 5 ; SummProp = CLIP(SummProp) & ' пятнадцать'
     OF 6 ; SummProp = CLIP(SummProp) & ' шестнадцать'
     OF 7 ; SummProp = CLIP(SummProp) & ' семнадцать'
     OF 8 ; SummProp = CLIP(SummProp) & ' восемнадцать'
     OF 9 ; SummProp = CLIP(SummProp) & ' девятнадцать'
     .
   of 2 ; SummProp = clip(SummProp) & ' двадцать'
   OF 3 ; SummProp = CLIP(SummProp) & ' тридцать'
   OF 4 ; SummProp = CLIP(SummProp) & ' сорок'
   OF 5 ; SummProp = CLIP(SummProp) & ' пятьдесят'
   OF 6 ; SummProp = CLIP(SummProp) & ' шестьдесят'
   OF 7 ; SummProp = CLIP(SummProp) & ' семьдесят'
   OF 8 ; SummProp = CLIP(SummProp) & ' восемьдесят'
   OF 9 ; SummProp = CLIP(SummProp) & ' девяносто'
   .
.

if UNI <> 0 and DEC <> 1
   case UNI
   of 1 ; SummProp = clip(SummProp) & ' один'
   OF 2 ; SummProp = CLIP(SummProp) & ' два'
   OF 3 ; SummProp = CLIP(SummProp) & ' три'
   OF 4 ; SummProp = CLIP(SummProp) & ' четыре'
   OF 5 ; SummProp = CLIP(SummProp) & ' пять'
   OF 6 ; SummProp = CLIP(SummProp) & ' шесть'
   OF 7 ; SummProp = CLIP(SummProp) & ' семь'
   OF 8 ; SummProp = CLIP(SummProp) & ' восемь'
   OF 9 ; SummProp = CLIP(SummProp) & ' девять'
   .
.
SummProp = left( SummProp )
return( SummProp )


R_UPPER  FUNCTION (Ch)  ! Перевод символа в верхний регистр
!============================================
CODE
  if Ch >= val('а') and Ch <= val('я')
    return( Ch-32 )
  else
    return( val( upper(Ch) ) )
  .

```

# Функция RCharPos

```
Функция RCharPos
Функция RCharPos находит позицию последнего вхождения заданного символа в строке.
Принимает два параметра:

Prm:SubString   String   - Искомый символ
Prm:String         String    - Строка, в которой производится поиск

Если длина Prm:SubString не равна 1-у символу, то функция сразу возвращает 0.

Прототип:  RCharPos Function(String,String),UShort

RCharPos  Function( Prm:SubString, Prm:String )
!===============================================
loc:LoopVar  Long

  Code

  If Len(Clip(Prm:SubString)) > 1
    Return(0)
  End
  Loop Loc:LoopVar = Len(Clip(Prm:String)) To 1 By -1
    If Prm:String[Loc:LoopVar] = Prm:SubString
      Return(Loc:LoopVar)
    End
  End
```

# Функция хеширования и шифрования Tandem DM

```
Функции хеширования и шифрования
Многих интересует как можно зашифровать/расшифровать какую-либо секретную информацию в своей программе.
Чтобы не пересказывать здесь основы криптографии отсылаю вас к первой статье цикла  "Криптография"
Андрея Беляева, опубликованной в журнале "Программист" №4 за 2002 год.  В этой статье приведен
алгоритм хеш-функции "Tandem DM" и алгоритмы шифрования текста на псевдокоде.  Я "перевел" эти алгоритмы
на язык Clarion и привожу здесь.

Функцию хеширования TandemDM можно применять для получения хеш-значения строки произвольной длины.
Я использую хеш-значения строк в качестве ключевых полей файла, по которым однозначно ищутся нужные строки.
Это позволяет очень сильно сократить длину ключевых полей. Такой прием применен, например, в моей
системе поддержки "многоязыковости" интерфейса программ MLang.  Конечно, можно применить эту
функцию и для того чтобы преобразовать пароль (легко запоминаемую строковую фразу) в ключ – большое число,
являющееся основным параметром шифрования.

TandemDM  FUNCTION (String str_, *Long r0, *Long r1) ! Хеш-функция "Tandem DM"
!=============================================================================
! Str_ - Входная строка, для которой надо вычислить хеш-функцию
! r0, r1 - два 64-битных числа для приема 128-битного результата
! Функция также возвращает результат в виде 8-символьной строки - RetStr

Num       USHORT
Str       STRING(2048)
RetStr    STRING(8)
HASH      BYTE,DIM(8),OVER(RetStr)
TMP       BYTE,DIM(4)
G         BYTE,DIM(4)
H         BYTE,DIM(4)
J         LONG
y         LONG
z         LONG
k0        LONG
k1        LONG
k2        LONG
k3        LONG

 CODE
 Str = Str_
 Num = len( clip(Str) ) / 4 + 1

 G[1]=0; G[2]=0; G[3]=0; G[4]=0;
 H[1]=0; H[2]=0; H[3]=0; H[4]=0;

 loop J=1 to Num
   ! TMP = EnCript( H, [G,PSWj] )
   y  = H[1]*256 + H[2]
   z  = H[3]*256 + H[4]
   k0 = G[1]*256 + G[2]
   k1 = G[3]*256 + G[4]
   k2 = val(Str[(J-1)*4+1])*256 + val(Str[(J-1)*4+2])
   k3 = val(Str[(J-1)*4+3])*256 + val(Str[(J-1)*4+4])
   EnCript( y, z, k0, k1, k2, k3 )
   TMP[1] = y / 256
   TMP[2] = y % 256
   TMP[3] = z / 256
   TMP[4] = z % 256

   ! H = H XOR TMP
   H[1] = bxor( H[1], TMP[1] )
   H[2] = bxor( H[2], TMP[2] )
   H[3] = bxor( H[3], TMP[3] )
   H[4] = bxor( H[4], TMP[4] )

   ! TMP = EnCript( G, [PSWj,TMP] )
   y  = bshift(G[1],8) + G[2]
   z  = bshift(G[3],8) + G[4]
   k0 = bshift(Str[J*4+1],8) + Str[J*4+2]
   k1 = bshift(Str[J*4+3],8) + Str[J*4+4]
   k2 = bshift(TMP[1],8) + TMP[2]
   k3 = bshift(TMP[3],8) + TMP[4]
   EnCript( y, z, k0, k1, k2, k3 )
   TMP[1] = y / 256
   TMP[2] = y % 256
   TMP[3] = z / 256
   TMP[4] = z % 256

   ! G = G XOR TMP
   G[1] = bxor( G[1], TMP[1] )
   G[2] = bxor( G[2], TMP[2] )
   G[3] = bxor( G[3], TMP[3] )
   G[4] = bxor( G[4], TMP[4] )
 .  ! end loop

  r0 = ((G[1]*256 + G[2])*256 + G[3])*256 + G[4]
  r1 = ((H[1]*256 + H[2])*256 + H[3])*256 + H[4]
  return( RetStr )


EnCript  PROCEDURE (*Long y,*Long z, Long k0,Long k1, Long k2, Long k3)
!=====================================================================
! Алгоритм шифрования: Tiny Encription Algorithm
! Шифруемый 64-разрядный блок помещается в y и z
! 128-разрядный ключ в k0, k1, k2, k3
! результат возвращается в переменных y и z.

Delta   LONG
A       LONG
Sum     LONG

 CODE
 Delta = 9E3779B9h
 Sum = 0

 loop A=1 to 32
   Sum += Delta
   y += bxor( bxor( bshift(z,4)+k0, z+Sum ), bshift(z,-5)+k1 )
   z += bxor( bxor( bshift(y,4)+k2, y+Sum ), bshift(y,-5)+k3 )
 .  ! end loop A

DeCript   PROCEDURE (*Long y,*Long z, Long k0,Long k1, Long k2, Long k3)
!=====================================================================
! Алгоритм разшифровки: Tiny Decription Algorithm

Delta   LONG
A       LONG
Sum     LONG

  CODE
 Delta = 9E3779B9h
 Sum = bshift( Delta, 5 )

 loop A=1 to 32
   z -= bxor( bxor( bshift(y,4)+k2, y+Sum ), bshift(y,-5)+k3 )
   y -= bxor( bxor( bshift(z,4)+k0, z+Sum ), bshift(z,-5)+k1 )
   Sum -= Delta
 .  ! end loop A


```

# Функция преобразования

```
! получить последний день месяца
 =day(DATE(MONTH(STA:DATE_ORDER)+1,1,YEAR(STA:DATE_ORDER))-1))

! получить первый день месяца
 =day(DATE(MONTH(STA:DATE_ORDER),1,YEAR(STA:DATE_ORDER))))

! получить количество лет
KOL_YEAR=INT((TODAY()-CUSTOMER_AGE)/365.25)

! Вычислить первый день месяца
FirstOfMonth = DATE(MONTH(TODAY()),1,YEAR(TODAY()))  

--------------------------------------------
Colors
Tools: RAD Clarion for Windows, version 6.x

Age
Library: CHSTD, AgeEx()
DateEnd += 1
if DateBegin > 3 and DateBegin < DateEnd
  Days = day(DateEnd) - day(DateBegin)
  Months = month(DateEnd) - month(DateBegin)
  Years = year(DateEnd) - year(DateBegin)
  if Days < 0
    Days = day(date(month(DateBegin)+1,01,year(DateBegin))-1) + Days
    Months -= 1
  end
  if parMonths < 0
    Years -= 1
    Months = 12 + Months
  end
else
  Years = 0
  Months = 0
  Days = 0
end
DateBegin - (in) begin of period
DateEnd - (in) end of period
Years, Months, Days - (out) age as "years,months,days"

-------------------------------------------------------

Leap-year (the bissextile)
Library: CHSTD, CheckBissextile()
Variant 1:
if (date(12, 31, Year) - date(1, 1, Year) + 1) = 366
  return true
end
return false
Variant 2:
if month((date(2, 29, Year)) = 2
  return true
end
return false

-------------------------------------------------------
Year - (in)

Quarter of a year
Library: CHSTD, QuarterOfYear()
Quarter = int(Month/4+0.5)+1
Month - (in) from 1 to 12

------------------------------------------------------
First day of period
Library: CHSTD, FirstDay()
Millennium:
Date = date(1, 1, int(Year/1000)*1000)
Century:
Date = date(1, 1, int(Year/100)*100)
Year:
Date = date(1, 1, Year)
Quarter:
Date = date(Quarter*3-2, 1, Year)
Month:
Date = date(Month, 1, Year)
Year, Month - (in)

------------------------------------------------------
Last day of period
Library: CHSTD, LastDay()
Millennium:
Date = date(12, 31, int(Year/1000)*1000+999)
Century:
Date = date(12, 31, int(Year/100)*100+99)
Year:
Date = date(12, 31, Year)
Quarter:
Date = date(Quarter*3+1, 1, Year) - 1
Month:
Date = date(Month+1, 1, Year) - 1
Year, Month - (in)

----------------------------------------------------------
Convert h:m:s.o in Clarion-time
Library: CHSTD, Time()
Time += H * 360000
Time += M * 6000
Time += S * 100
Time += O
if Time > 0 then Time += 1 end
H,M,S,O - (in) hours, minutes, seconds, 100-th split second

----------------------------------------------------------
Number of weeks in the period
Library: CHSTD, WeekNumer()
DayOfWeek1 = DateBegin % 7
if DayOfWeek1 = 0
  DayOfWeek1 = 7
end
if DayOfWeek1 <> 1
  DateBegin += 7 - DayOfWeek1 + 1
end
DayOfWeek2 = DateEnd % 7
DateEnd -= DayOfWeek2

-----------------------------------------------------------
! Number of full weeks
if DateBegin < DateEnd
  WeekNumer = int((DateEnd - DateBegin+1)/7)
end
! + not full week at beginning of the period
if DayOfWeek1 <> 1
  WeekNumer += 1
end
! + not full week at end of the period
if DayOfWeek2 <> 0
  WeekNumer += 1
end
DateBegin,DateEnd - (in)
DayOfWeek1,DayOfWeek2 - local variables

--------------------------------------------------
CMYK to RGB
Library: CHSTD, CMYKtoColor()
if (C + K) < 255
  Red = 255 - (C + K)
end
if (M + K) < 255
  Green = 255 - (M + K)
end
if (Y + K) < 255
  Blue = 255 - (Y + K)
end

C,M,Y,K - CMYK
Red, Green, Blue - RGB

---------------------------------------------------
RGB to CMYK
Library: CHSTD, ColorToCMYK()
C = 255 - Red
M = 255 - Green
Y = 255 - Blue
K = CHOOSE(C < M, C, M)
if Y < K
  K = Y
end
if K > 0
  C -= K
  M -= K
  Y -= K
end

! Correct For Print
Min = CHOOSE(C < M, C, M)
if Y < Min
  Min = Y
end
if Min + K > 255
  Min = 255 - K
end
  C -= Min
  M -= Min
  Y -= Min
  K += Min
end


C,M,Y,K - CMYK
Red, Green, Blue - RGB
Min - local variable

-------------------------------------------------
HSL to Color
Library: CHSTD, HSLtoColor()
if L <> 0
  if S = 0
    Red = L
    Green = L
    Blue = L
  else
    if L <= 128
      V2 = L * (255 + S) / 255
    else
      V2= L + S - (L*S)/255
    end
    V1 = 2*L - V2
    H0 = H + 256/3
    do Calc
    Red = V0
    H0 = H
    do Calc
    Green = V0
    H0 = H - 256/3
    do Calc
    Blue = V0
  end
end

Calc routine
  if H0 < 0
    H0 += 255
  elsif H0 > 255
    H0 -= 255
  end
  if H0 < 255/6
    V0 = V1 + ((V2-V1)*H0 + 255/12)/(255/6)
  elsif H0 < 128
    V0 = V2
  elsif H0 < 255*2/3
    V0 = V1 + ((V2-V1)*(255*2/3 - H0) + 255/12)/(255/6)
  else
    V0 = V1
  end
H,S,L - HSL
Red, Green, Blue - RGB
H0,V0,V1,V2 - local variables

-------------------------------------------------
Color to HSL
Library: CHSTD, ColorToHSL()
Max = fRGBmax(RGB) ! maximum value of color
Min = fRGBmin(RGB) ! minimum value of color
if Max = Min
  H = 0
  S = 0
else
  A1 = Max/255 + Min/255
  S1 = Max/255 - Min/255
  L = (Max + Min) / 2
  if L < 128
    S = (S1 / A1) * 255
  else
    S = (S1 / (2-S1) ) * 255
  end
  case Max
  of Red
    H = ( (Green-Blue)/255 / S1 ) * 60
  of Green
    H = ( (2 + (Blue-Red)/255) / S1 ) * 60
  of Blue
    H = ( (4 + (Red-Green)/255) / S1 ) * 60
  end
  if H < 0
    H += 255
  end
end
H,S,L - HSL
Red, Green, Blue - RGB
A1,S1,Min,Max - local variables

-----------------------------------------------
HSV to Color
Library: CHSTD, HSVtoColor()
if S = 0     ! achromatic: shades of gray
  Red = V
  Green = V
  Blue = V
else         ! chromatic color
  F0 = H/255*6 - int(H/255*6)
  VS = V * S / 255
  P0 = V - int(VS)
  Q0 = V - int(VS * F0)
  T0 = V - int(VS * (1-F0))
  case int(H/255*6)
  of 0
    Red = V
    Green = T0
    Blue = P0
  of 1
    Red = Q0
    Green = V0
    Blue = P0
  of 2
    Red = P0
    Green = V
    Blue = T0
  of 3
    Red = P0
    Green = Q0
    Blue = V
  of 4
    Red = T0
    Green = P0
    Blue = V
  of 5
    Red = V
    Green = P0
    Blue = Q0
  end
end
H,S,V - HSV
Red, Green, Blue - RGB
F0,VS,P0,Q0,T0 - local variables

------------------------------------------------
Color to HSV
Library: CHSTD, ColorToHSV()
V = fRGBmax(RGB)
Delta = V - fRGBmin(RGB)
S = choose(V=0, 0, Delta * 255 / V)
if S = 0
  H = 0
else
  if Red = V
    H0 = (Green - Blue) / Delta
  elsif Green = V
    H0 = 2 + (Blue-Red) / Delta
  else
    H0 = 4 + (Red-Green) / Delta
  end
  if H0 < 0
    H0 += 6
  end
  H = H0 * 255/6
end
H,S,V - HSV
Red, Green, Blue - RGB
Delta,H0 - local variables

------------------------------------------------
YUV to Color
Library: CHSTD, YUVtoColor()
Red = round(1.164383 * (Y-16) + 1.596027 * (parV-128))
Blue = round(1.164383 * (Y-16) - (0.391762 * (parU-128)) - (0.812968 * (parV-128)))
Green = round(1.164383 * (Y-16) + 2.017232 * (parU-128))
Y,U,V - YUV
Red, Green, Blue - RGB

-------------------------------------------------
Color to YUV
Library: CHSTD, ColorToYUV()
K = 0.299 * Red + 0.587 * Green + 0.114 * Blue
Y = round(0.859 * K) + 16
U = round(0.496 * (Blue - K)) + 128
V = round(0.627 * (Red - K)) + 128
Y,U,V - YUV
Red, Green, Blue - RGB

-------------------------------------------------

```

# Функция KeyBrd виртуальная клавиатура

```
KeyBrd      Function(<STRING psString>)

esPos     string('<187>')      !Current position indicator
asChar    string(47)           !String to hold key charactors
afCapLoc  byte                 !Is Caplocks down?
afShift   byte                 !Is shift key down?
afPassed  byte                 !Are we using a passed string?
afIns     byte                 !Are we in insert mode?
alDim     long                 !Current position
aiRtn     short                !Used to hold value of ACCEPTED
afReset   byte                 !Update the keyboard display?
abLoop    byte                 !Generic loop counter
abHold    byte                 !Hold bucket for character covered by esPOS
alStrSize long                 !Maximum length of string
alEOS     long                 !Last charactor currently in string
?Text     equate(10)           !Value for create
asString  string( 250 )        !Working string when none is passed.

window WINDOW,AT(,,270,125),GRAY
       TEXT,AT(0,0,270,36),USE(asString,?Text),FONT(,,COLOR:Yellow,),COLOR(COLOR:Blue)
       BUTTON,AT(0,36,18,18),USE(?S01,101)
       BUTTON,AT(18,36,18,18),USE(?S02,102)
       BUTTON,AT(36,36,18,18),USE(?S03,103)
       BUTTON,AT(54,36,18,18),USE(?S04,104)
       BUTTON,AT(72,36,18,18),USE(?S05,105)
       BUTTON,AT(90,36,18,18),USE(?S06,106)
       BUTTON,AT(108,36,18,18),USE(?S07,107)
       BUTTON,AT(126,36,18,18),USE(?S08,108)
       BUTTON,AT(144,36,18,18),USE(?S09,109)
       BUTTON,AT(162,36,18,18),USE(?S10,110)
       BUTTON,AT(180,36,18,18),USE(?S11,111)
       BUTTON,AT(198,36,18,18),USE(?S12,112)
       BUTTON,AT(216,36,18,18),USE(?S13,113)
       BUTTON,AT(234,36,18,18),USE(?S14,114)
       BUTTON,AT(27,54,18,18),USE(?S15,115)
       BUTTON,AT(45,54,18,18),USE(?S16,116)
       BUTTON,AT(63,54,18,18),USE(?S17,117)
       BUTTON,AT(81,54,18,18),USE(?S18,118)
       BUTTON,AT(99,54,18,18),USE(?S19,119)
       BUTTON,AT(117,54,18,18),USE(?S20,120)
       BUTTON,AT(135,54,18,18),USE(?S21,121)
       BUTTON,AT(153,54,18,18),USE(?S22,122)
       BUTTON,AT(171,54,18,18),USE(?S23,123)
       BUTTON,AT(189,54,18,18),USE(?S24,124)
       BUTTON,AT(207,54,18,18),USE(?S25,125)
       BUTTON,AT(225,54,18,18),USE(?S26,126)
       BUTTON,AT(36,72,18,18),USE(?S27,127)
       BUTTON,AT(54,72,18,18),USE(?S28,128)
       BUTTON,AT(72,72,18,18),USE(?S29,129)
       BUTTON,AT(90,72,18,18),USE(?S30,130)
       BUTTON,AT(108,72,18,18),USE(?S31,131)
       BUTTON,AT(126,72,18,18),USE(?S32,132)
       BUTTON,AT(144,72,18,18),USE(?S33,133)
       BUTTON,AT(162,72,18,18),USE(?S34,134)
       BUTTON,AT(180,72,18,18),USE(?S35,135)
       BUTTON,AT(198,72,18,18),USE(?S36,136)
       BUTTON,AT(216,72,18,18),USE(?S37,137)
       BUTTON,AT(45,90,18,18),USE(?S38,138)
       BUTTON,AT(63,90,18,18),USE(?S39,139)
       BUTTON,AT(81,90,18,18),USE(?S40,140)
       BUTTON,AT(99,90,18,18),USE(?S41,141)
       BUTTON,AT(117,90,18,18),USE(?S42,142)
       BUTTON,AT(135,90,18,18),USE(?S43,143)
       BUTTON,AT(153,90,18,18),USE(?S44,144)
       BUTTON,AT(171,90,18,18),USE(?S45,145)
       BUTTON,AT(189,90,18,18),USE(?S46,146)
       BUTTON,AT(207,90,18,18),USE(?S47,147)
       BUTTON,AT(252,36,18,18),USE(?S48,148),ICON(ICON:VCRback)
       BUTTON('TAB'),AT(0,54,27,18),USE(?S49,149)
       BUTTON('Cap Lock'),AT(0,72,36,18),USE(?S50,150)
       BUTTON('ENTER'),AT(234,72,36,18),USE(?S51,151)
       BUTTON('SHIFT'),AT(0,90,45,18),USE(?S52,152)
       BUTTON('SHIFT'),AT(225,90,45,18),USE(?S53,153)
       BUTTON('SPACE'),AT(81,108,108,18),USE(?S58,154)
       BUTTON('<<---'),AT(0,108,45,18),USE(?S59,155)
       BUTTON('--->'),AT(225,108,45,18),USE(?S60,156)
       BUTTON('INS'),AT(252,54,18,18),USE(?61,157)
     END

    code
    open( window )             !Open up the shop for business
    if omitted(1)              !If nothing was passed to us,
       afPassed = false        !State as much
       ?Text{Prop:Use} = asString  !use the default string,
       alStrSize = 250         !and set the maximum to 250.
    else
       afPassed = true         !If we were passed something,
       ?Text{Prop:Use} = psString !use it
       alStrSize = len( psString )!and set our max to its size.
       alEOS = len( clip( psString ) )
    END !if not omitted(1)
    alDim = 1                  !Start out at the first charactor
    if afPassed then alDim += len( clip( psString ) ).
    afReset = true
    aiRtn = 200
    do Put_Char
    display
    accept
       if afPassed
          if psString[alDim] = esPos
             if abHold
                psString[alDim] = Chr( abHold )
             else
                psString[alDim] = ''
             END ! if asHold
          END ! if asString[alDim] = esPos
       else
          if asString[alDim] = esPos
             if abHold
                asString[alDim] = Chr( abHold )
             else
                asString[alDim] = ''
             END ! if asHold
          END ! if asString[alDim] = esPos
       END !if afPassed

       afReset = false
       case event()
       of event:Accepted
         aiRtn = accepted()
         if aiRtn < 100 then cycle.
         do Put_Char
       end! Case
    END! accept

Put_Char      routine
    abHold = 0
    case aiRtn
    of 200       ! Do nothing value used during startup.
       if afPassed
          if psString[ alDim ]
             abHold = val( psString[ alDim ] )
             psString[ alDim ] = ''
          End !if asString[ alDim ]
       else
          if asString[ alDim ]
             abHold = val( asString[ alDim ] )
             asString[ alDim ] = ''
          End !if asString[ alDim ]
       END
    of 157       !Insert/Overtype toggle
       if afIns                !If we were in insert mode,
          afIns = false        !set to Overtype and show it on the button.
          aiRtn{Prop:Text} = 'INS'
       else                    !If we were in overtype (Default) mode,
          afIns = true         !set to Insert and show it on the button.
          aiRtn{Prop:Text} = 'OVR'
       end !if afIns
    of 156       ! Moving to the RIGHT
       if alDim < alStrSize
          alDim +=1
          if afPassed
             abHold = Val( psString[ alDim  ] )
             psString[ alDim  ] = ''
          else
             abHold = Val( asString[ alDim  ] )
             asString[ alDim  ] = ''
          END
       END ! if alDim <
    of 155       ! Moving to the LEFT
       if alDim > 1 then alDim -=1.
       if afPassed
          abHold = Val( psString[ alDim  ] )
          psString[ alDim  ] = ''
       else
          abHold = Val( asString[ alDim  ] )
          asString[ alDim  ] = ''
       END
    of 152 orof 153 !Shift
       afShift = 1 - afShift
       afReset = true
    of 150      !CapLock
       afCapLoc = 1 - afCapLoc
       afShift = False
       afReset = true
    of 148         !BackSpace
       if alDim > 1
         alDim -= 1
         if afPassed
            psString[ alDim ] = ''
         else
            asString[ alDim ] = ''
         END
       .  !if alDim > 1
    of 149         !Tab
       if alDim % 8
         alDim += alDim % 8
      else
         alDim += 8
       .  !if alDim % 8
       if alDim > alStrSize then alDim = alStrSize.
    of 151
       if afPassed
          return( psString )
       else
          return( asString )
       END
    else
       if alDim <= alStrSize
          if afPassed
             if afIns then psString[alDim+1 : alEOS+1] = psString[alDim : alEOS].
             psString[ alDim ] = aiRtn{ prop:Text }
             if aiRtn = 154 then psString[ alDim ] = ''.   !Handles SPACE
          else
             if afIns then asString[alDim+1 : alEOS+1] = asString[alDim : alEOS].
             asString[ alDim ] = aiRtn{ prop:Text }
             if aiRtn = 154 then asString[ alDim ] = ''.   !Handles SPACE
          END
       END
       if alDim < alStrSize
          alDim += 1
          if afPassed
             if psString[ alDim ]
                abHold = val( psString[ alDim ] )
                psString[ alDim ] = ''
             End !if asString[ alDim ]
          else
             if asString[ alDim ]
                abHold = val( asString[ alDim ] )
                asString[ alDim ] = ''
             End !if asString[ alDim ]
          END
       end !if alDim < 250
       if afShift then afReset = true.
       afShift = false
    .  !case aiRtn
    if afReset then do Set_Char.
    if afPassed
       if psString[alDim] = '' then psString[alDim] = esPos.
       alEOS = len( Clip( psString ) )
    else
       if asString[alDim] = '' then asString[alDim] = esPos.
       alEOS = len( Clip( asString ) )
    end
    display

Set_Char      routine
     if ( afCapLoc + afShift )
       asChar = '~!@#$%^&*()_+|QWERTYUIOP{{}ASDFGHJKL:"ZXCVBNM<<>?'
     else
       asChar = '`1234567890-=\qwertyuiop[]asdfghjkl;''zxcvbnm,./'
     end !IF
     abLoop = 0
     loop
        abLoop += 1
        if asChar[abLoop] = '&'
           (abLoop + 100){prop:text} = '&&'
        else
           (abLoop + 100){prop:text} = asChar[abLoop]
        end! if asChar[abLoop] = '&'
        if abLoop => 47 then break.
     .  !loop
```

# Функция Конвертирует строку из формата  Ansi в формат HTML

```
!-------------------------------------------------------------
!    map
!AnsiToHTML   FUNCTION(string),string
!    end
!
! Конвертирует строку из формата  Ansi в формат HTML
!-------------------------------------------------------------
AnsiToHTML    FUNCTION(AnsiString) ! Convert AnsiString to HTML line
Lok:Line String(1024)
Space    Byte
CODE
  MaxFou# = Len(Clip(AnsiString))
  Lok:Line = ''
  space = 1
  Loop co1# = 1 to MaxFou#
    If AnsiString[co1#] <> ' ' Then Break.
  End
  ci# = 0
  Loop co# = co1# to MaxFou#
    If AnsiString[co#] = ' '
      Space = 0
      Cycle
    End
    If Space = 0
      ci# += 1
      Lok:Line[ci#] = ' '
    End
    Space = 1
    ci# += 1
      Case Val(AnsiString[co#])
      OF 10  ! lf
        Lok:Line[ci#] = '<<'
        Lok:Line = Clip(Lok:Line) & 'BR>'
        ci# += 3
      OF 12
        ci# -= 1
      OF 13  ! gr
        ci# -= 1
      OF 34   !  '"'
        Lok:Line[ci#] = '&'
        Lok:Line = Clip(Lok:Line) & 'quot;'
        ci# += 5
      OF 38   !  '&'
        Lok:Line[ci#] = '&'
        Lok:Line = Clip(Lok:Line) & 'amp;'
        ci# += 4
      OF 60  ! '<'
        Lok:Line[ci#] = '&'
        Lok:Line = Clip(Lok:Line) & 'lt;'
        ci# += 3
      OF 62  ! '>'
        Lok:Line[ci#] = '&'
        Lok:Line = Clip(Lok:Line) & 'gt;'
        ci# += 3
      ELSE
        Lok:Line[ci#] = AnsiString[co#]
      END
  End
  IF (LOK:Line = '')
    Lok:Line = '&nbsp;'
  END
  RETURN(LOK:Line)
```

# Функция Конвертирует строку из формата  Ansi в формат RTF

```
!-------------------------------------------------------------
!    map
!AnsiToRTF   FUNCTION(string),string
!    end
!
! Конвертирует строку из формата  Ansi в формат RTF
!-------------------------------------------------------------
AnsiToRTF    FUNCTION(AnsiString) ! Convert AnsiString to RTF line
RTFHexStr           String('0123456789abcdef')
Lok:Line String(1024)
Space    Byte
CODE
  MaxFou# = Len(Clip(AnsiString))
  Lok:Line = ''
  space = 1
  Loop co1# = 1 to MaxFou#
    If AnsiString[co1#] <> ' ' Then Break.
  End
  ci# = 0
  Loop co# = co1# to MaxFou#
    If AnsiString[co#] = ' '
      Space = 0
      Cycle
    End
    If Space = 0
      ci# += 1
      Lok:Line[ci#] = ' '
    End
    Space = 1
    ci# += 1
    If VAl(AnsiString[co#]) >= 128
      Lok:Line[ci#] = '\'
      ci# += 1
      Lok:Line[ci#] = ''''
      ci# += 1
      Lok:Line[ci#] = RTFHexStr[VAl(AnsiString[co#])/16+1]
      ci# += 1
      Lok:Line[ci#] = RTFHexStr[(VAl(AnsiString[co#]) % 16) +1]
    Else
      Case Val(AnsiString[co#])
      OF 9  ! Tab
        Lok:Line[ci#] = '\'
        Lok:Line = Clip(Lok:Line) & 'tab '
        ci# += 4
      OF 10  ! lf
        Lok:Line[ci#] = '\'
        Lok:Line = Clip(Lok:Line) & 'par '
        ci# += 4
      OF 12
        Lok:Line[ci#] = '\'
        Lok:Line = Clip(Lok:Line) & 'sect '
        ci# += 5
      OF 13  ! gr
        ci# -= 1
      OF 92   !  '\'
        Lok:Line[ci#] = '\'
        ci# += 1
        Lok:Line[ci#] = '\'
      OF 123  ! '{'
        Lok:Line[ci#] = '\'
        ci# += 1
        Lok:Line[ci#] = '{{'
      OF 125  ! '}'
        Lok:Line[ci#] = '\'
        ci# += 1
        Lok:Line[ci#] = '}'
      ELSE
        Lok:Line[ci#] = AnsiString[co#]
      END
    End
  End
  RETURN(LOK:Line)
```

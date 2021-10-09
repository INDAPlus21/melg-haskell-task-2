## Fråga 1
Eftersom Haskell har lazy evaluation kör den inte funktionen när den defineras, utan bara den delen som används. I detta fall skapar den en oändlig talföljd men tar endast ut dom 1000000 första talen och räknar därmed inte ut fler än 1000000 värden.

## Fråga 2
Rekursion är att en funktion kallar en eller fler nya funktion, det blir alltså ett funktionsträd. När botten har nåtts skickas returvärdet uppåt tills det har nått orginal funktionen.

## Fråga 3
I haskell så kan en funktion defineras på olika sätt för olika värden medans i tex JS kan en funktion som tar in olika parametrar bara defineras på ett sätt.
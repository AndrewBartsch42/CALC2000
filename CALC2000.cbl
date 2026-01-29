       identification division.

       program-id. calc2000.
      * Programmer.: Andrew Bartsch
      * Date : 1/29/2026
      * GithubURL:
      * Description: 
      * This program calculates future values for an investment
      * and doubles the investment amount twice 
       environment division.

       input-output section.

       data division.

       file section.

       working-storage section.

       01  input-values.

           05  number-entered              pic 9        value 1.
           05  investment-amount           pic 9999  value 1000.
           05  number-of-years             pic 99      value 10.
           05  yearly-interest-rate        pic 99v9   value 5.5.

       01  work-fields.

           05  future-value                pic 9(7)v99.
           05  year-counter                pic 999.
           05  edited-whole-value         pic zz,zzz,zz9.
           05  edited-decimal-value       pic zzz,zzz.99.

       procedure division.

       000-calculate-future-values.
           display "Calculating Future Values".
           perform 100-calculate-future-value.
           compute investment-amount = investment-amount * 2
           perform 100-calculate-future-value.
           compute investment-amount = investment-amount * 2
           perform 100-calculate-future-value.
           stop run.

       100-calculate-future-value.
           if number-entered = 1
               move investment-amount to future-value
               move 1 to year-counter
               perform 120-calculate-next-fv
                   until year-counter > number-of-years
               move future-value to edited-whole-value. 
           perform 140-display-values.

       120-calculate-next-fv.

           compute future-value rounded =
               future-value +
                   (future-value * yearly-interest-rate / 100).
           add 1 to year-counter.
       
       140-display-values.
           move edited-whole-value to edited-decimal-value. 
           display "--------------------------------".
           display "Investment Amount = " investment-amount.
           display "Number of years   = " number-of-years.
           display "Yearly Interest   = " yearly-interest-rate.
           display "Future value      = " edited-decimal-value.     

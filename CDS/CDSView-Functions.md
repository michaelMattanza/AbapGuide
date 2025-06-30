# FUNCTION

Functions exist to improve code quality. They can be *SQL Functions* or *Built-In Functions*.

- *SQL Functions*
These are basic and inflexible functions that return a logical or boolean result. This category of functions can be further subdivided into two other categories: numeric and string.
  - *Numeric*

| Function            | Definition                                  | Output                            |
|---------------------|---------------------------------------------|-----------------------------------|
| ABS(arg)            | Absolute value of the number arg            | Absolute value                    |
| CEIL(arg)           | Rounding a decimal number up                | Number rounded up                 |
| DIV(arg1, arg2)     | Conventional division (integers)            | Quotient                          |
| DIVISION(arg1, arg2, dec) | Division with decimals                      | Result rounded to decimals        |
| MOD(arg1, arg2)     | Conventional modulo operation               | Remainder of the division         |
| FLOOR(arg)          | Rounding a decimal number down              | Number rounded down               |
| ROUND(arg, pos)     | Rounding a number with decimals             | Number rounded by decimal         |

  - *String*

| Function                           | Definition                                                                        |
|------------------------------------|-----------------------------------------------------------------------------------|
| LENGTH(arg)                        | Length of the string                                                              |
| INSTR(arg1, arg2)                  | Position of one string within another                                             |
| CONCATENATE(arg1, arg2)            | Concatenates two strings                                                          |
| CONCATENATE WITH SPACE(arg1, arg2) | Concatenates two strings with a space in between                                  |
| LEFT(arg1, arg2)                   | Takes the first n characters of arg1                                              |
| LOWER(arg1)                        | Converts the passed string to lowercase                                           |
| LPAD(arg1, arg2, arg3)             | Pads arg1 with string arg3 on the left to reach length arg2                       |
| RPAD(arg1, arg2, arg3)             | Pads arg1 with string arg3 on the right to reach length arg2                      |
| LTRIM(arg1, arg2)                  | Removes the string indicated in arg2 from arg1 (the first from the left)          |
| RTRIM(arg1, arg2)                  | Removes the string indicated in arg2 from arg1 (the first from the right)         |
| REPLACE(arg1, arg2, arg3)          | Replaces substring arg2 in arg1 with string arg3                                  |
| SUBSTRING(arg1, arg2, arg3)        | Reads n characters (arg3) from arg1 starting from position arg2                   |
| UPPER(arg1)                        | Converts all letters in the passed string to uppercase                            |

- *Built-In Functions*
  - *Unit Conversion Functions*
    **Syntax**
    `unit_conversion( quantity => brgew,`
    `                  source_unit => meins,`
    `                  target_unit => gewei`
    `                  )`
    **Rules**
    If there are no relationships between `source_unit` and `target_unit`, there will be a dump.

  - *Currency Conversion Functions*
    **Syntax**
    `currency_conversion( amount => a.price,`
    `                  source_currency => a.currency,`
    `                  target_currency => :p_to_curr,`
    `                  exchange_rate_date => :p_conv_date`
    `                  )`
    **Rules**
    Does not yield precise results.

  - *Decimal Shift*
    **Syntax**
    `decimal_shift( amount => :p_amt, currency => a.currency )`
    **Rules**
    Performs the conversion based on internal data (table TCURX) -> Needs further investigation.

  - *Date Functions*
    - *Add Days*
    **Syntax**
    `dats_add_days(a.fldate, :p_add_days , 'INITIAL')`
    **Rules**
    The first parameter is the date, the second is the days to add, the third is for error handling (INITIAL, FAIL, NULL, INITIAL, UNCHANGED).

    - *Add Months*
    **Syntax**
    `dats_add_months(a.fldate, :p_add_months, 'NULL')`
    **Rules**
    Similar to days, the first parameter is the date, the second is the months to add, the third is for error handling.

    - *Days between two dates*
    **Syntax**
    `dats_days_between (a.fldate, $parameters.p_curr_date )`
    **Rules**
    The first parameter is the start date, the second is the end date.

    - *Date validation*
    **Syntax**
    `dats_is_valid(a.fldate)`
    **Rules**
    Checks if the date is valid (returns 1 or 0).

> NB
Some of the built-in functions contain bugs.

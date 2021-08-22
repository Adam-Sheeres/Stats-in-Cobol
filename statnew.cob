      *> Program: Data Statistics
      *> By: Adam Sheeres-Paulicpulle
      *> Purpose: Calculate several statistical numbers based on file
       identification division.
       program-id. statnew.


       environment division.
       input-output section.
       file-control.
       select input-file assign to dynamic fname-inp
           organization is line sequential.
       select output-file assign to dynamic fname-out
           organization is line sequential.


       data division. 
       file section. 
       FD  input-file.
           01  input-rec       pic x(80) justified right.
       FD  output-file.
           01  output-line     pic x(80).


       working-storage section. 
       77  feof        pic a(1).
       77  fname-inp   pic x(30).
       77  fname-out   pic x(30).
       77  total-sum   pic s9(14)V9(4) usage is comp-3.
       77  num-count   pic s9999 usage is comp. 
       77  mean        pic s9(14)v9(4) usage is comp-3.
       77  i           pic s9999 usage is comp.
       77  j           pic s9999 usage is comp.
       77  j1          pic 9999.
       77  temp        pic S9(17)V9(20) usage is comp-3.
       77  temp2       pic S9(20)V9(14) usage is comp-3.
      *> Temp and Temp 2 differ by decimal places, needed for certain
      *> calculations

       01  file-info.
           05  file-size   pic X(8) comp-x.
           05  file-date.
               10  file-day        pic X(8) comp-x.
               10  file-month      pic X(8) comp-x.
               10  file-year       pic X(8) comp-x.
           05  file-time.
               10  file-hours      pic X(8) comp-x.
               10  file-minutes    pic X(8) comp-x.
               10  file-seconds    pic X(8) comp-x.
               10  file-mseconds   pic X(8) comp-x.

      *> More definitions for formatting output
       01  array-area.
           02 input-array  pic S9(20)V9(4) usage is comp-3
           occurs 1000 times.
       
       01  rec-len      pic 9(15) comp.

       01  input-value.
           02 input-val     pic s9(14)v9(4).
           02 filler        pic x(62).

       01  title-line.
           02 filler       pic x(45) value
           '  Means, Standard Deviation, Median, Variance'.
       01  under-line.
           02 filler       pic x(48) value
           '------------------------------------------------'.
        01 col-heads.
           02 filler       pic X(48) value '                   DATA VALUES                  '.
       01  data-line.
           02 filler       pic X(5) values spaces.
           02 out-x        pic -(32)9.9(4).


      *> Section contains the output of the program to write to the file
       01 print-line-1.
           02 filler   pic X(22) value ' Mean =               '.
           02 out-mean         pic -(15)9.9(4).
       01 print-line-2.
           02 filler   pic X(22) value ' Standard Deviation = '.
           02 out-standard     pic -(15)9.9(4).
       01 print-line-3.
           02 filler   pic X(22) value ' Geometric Mean =     '.
           02 out-geometric    pic -(15)9.9(4).
       01 print-line-4.    
           02 filler   pic x(22) value ' Harmonic Mean =      '.
           02 out-harmonic     pic -(15)9.9(4).
       01 print-line-5.
           02 filler   pic x(22) value ' Median =             '.
           02 out-medi         pic -(15)9.9(4).
       01 print-line-6.
           02 filler   pic x(22) value ' Variance =           '.
           02 out-variance     pic -(15)9.9(4).


      *> Start of procedure division. 
       procedure division.
       display "Input file name? "
           accept fname-inp.
       display "Output file name? "
           accept fname-out.
           perform file-check.

      *> Open files and write headers
           open input input-file, output output-file.
           write output-line from title-line after advancing 0 lines.
           write output-line from under-line after advancing 1 lines.
           write output-line from col-heads  after advancing 1 lines.
           write output-line from under-line after advancing 1 lines.
           compute total-sum = 0
           compute num-count = 0
           perform input-loop until feof='Y'
           perform main.
       

       file-check.
      *> Checks to see if the file exists
      *> This code was adapted from: 
      *> https://craftofcoding.wordpress.com/2021/03/22/coding-
      *> cobol-checking-a-file-exists/
           call "CBL_CHECK_FILE_EXIST" using fname-inp file-info.
           if return-code not = 0 then
               display "ERROR: FILE " fname-inp (1:20) " DOES NOT EXIST"
           end-if.


       input-loop.
      *> Read input, and decide how long the input is
      *> This code was adapted from: 
      *> https://craftofcoding.wordpress.com/2021/03/23/coding-cobol-
      *> a-bubblesort/
           read input-file into input-value
               at end move 'Y' to feof
               not at end
                   add 1 to num-count
                   move input-val to input-array(num-count), out-x
                   compute total-sum = total-sum + input-array(num-count)
      *> Write output to file as it is read
                       write output-line from data-line 
                       after advancing 1 line
           end-read.


       main.
      *> Calculate all the different needed variables
           divide num-count into total-sum giving mean rounded.
           perform mean-loop varying i from 1 by 1 until i > num-count.

           compute temp2 = 0.
           perform standard-deviation varying i from 1 by 1 until i > num-count.

           compute temp2 = 0.
           perform geometric-mean varying i from 1 by 1 until i > num-count.

           compute temp = 0.
           perform harmonic-mean varying i from 1 by 1 until i > num-count.

           compute temp2 = 0.
           perform median.

           write output-line from under-line after advancing 1 line.
           perform write-output.
           
       perform finish.


       write-output.
      *> Writes all the necessary output for calculated values
           move mean to out-mean.
           write output-line from print-line-1 after advancing 1 line.
           write output-line from print-line-2 after advancing 1 line.
           write output-line from print-line-3 after advancing 1 line.
           write output-line from print-line-4 after advancing 1 line.
           write output-line from print-line-5 after advancing 1 line.
           write output-line from print-line-6 after advancing 1 line.


       standard-deviation.
      *> Calculates the standard deviation of the sample input
           if i < num-count then
               compute temp2 = temp2 + (input-array(i) - mean) ** 2

            else 
               compute temp2 = temp2 + (input-array(i) - mean) ** 2
               compute temp2 = temp2 / num-count
      *> Variation is squared standard eviation
               move temp2 to out-variance
               compute temp2 = temp2 ** 0.5
      *> Temp2 has more decimals than stan-dev, allowing for greater 
      *> accuracy
               move temp2 to out-standard

           end-if.


       mean-loop.
      *> Calculates the mean of the number set
           compute temp = input-array(i) - mean.
           if temp < 0 then
               compute temp = temp*-1

           end-if.
           multiply temp by temp giving temp.
           add temp to total-sum.


       geometric-mean.
      *> Calculates the geometric mean of the number set
           if i < num-count then
      *> Uses log instead of multiplication, cutting down on variable
      *> length, allowing easier computation
               compute temp2 = temp2 + function log(input-array(i))
           else
               compute temp2 = temp2 + function log(input-array(i))
               compute temp2 = temp2/num-count
               compute temp2 =  2.71828182845904523536 ** temp2
               move temp2 to out-geometric
           end-if.


       harmonic-mean.
      *> Calculates harmonic mean
           if i < num-count then
               compute temp = temp + 1 / input-array(i)
           else 
               compute temp = temp + 1 / input-array(i)
               compute temp = num-count/temp
               move temp to out-harmonic
           end-if.


       bubbleSort.
      *> Performs bubble sort, to later find median
      *> This code was adapted from: 
      *> https://craftofcoding.wordpress.com/2021/03/23/coding-cobol-
      *> a-bubblesort/
           perform varying i from 1 by 1 until i > num-count
               perform varying j from 1 by 1 until j > num-count - i
               compute j1 = j + 1
                   if (input-array(j) > input-array(j1))
                       move input-array(j) to temp2
                       move input-array(j1) to input-array(j)
                       move temp2 to input-array(j1)
                   end-if
               end-perform
           end-perform.


       median.
      *> Finds the median of the set of numbers, includes cases for 
      *> both even and odd
           perform bubbleSort.
           compute temp2 rounded = num-count/2.
           compute i rounded = temp2
           if function mod (temp2, 2) = 0 then
      *> even
               compute temp2 = (input-array(i - 1) + input-array(i))
               compute temp2 = temp2 / 2
           else
      *> odd   
               compute temp2 = input-array(i)
           end-if.
           move temp2 to out-medi.


       finish.
       close input-file.
       close output-file.

       stop run.

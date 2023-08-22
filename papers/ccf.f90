! Simple CCF Processor
!
! This code is free.  You are also free (do whatever you want with this code).
! David Epstein appreciates any comments.
! Call (503) 549-2433 or write PO BOX 5, Camp Sherman, OR 97730
!
! BASIC IDEA
! ----------
! Since the Conditional Compilation Facility (CCF) is a subset of Fortran,
! a simple CCF processor is achieved by using a Fortran processor to
! execute the CCF statements.
!
! STEPS
! -----
! 1. Compile this CCF processor with your Fortran processor.
!
!    You now have a CCF processor.
!
! 2. Invoke this CCF processor and follow the prompts for required input.
! 3. Compile the ccf-temp file (ccftemp.f90 for dos and unix systems).
! 4. Execute the ccf-temp file to create the resulting output file.
!
! ALGORITHM
! ---------
! Turn !ccf$ lines into Fortran lines by replacing !ccf$ with blanks.
! Turn all other lines into output statements (include handling of !ccf*
! and !ccf$ lines as lines that CCF previously turned into comments).
! Special handling of CCF variables is required due to Fortran requirement
! that the specification-part precedes the execution-part.  All CCF variable
! declaration lines are buffered until written to a MODULE.  This MODULE
! is also used to handle initialization of CCF variables when the user
! wants to override initial values in the source.
!
! DESCRIPTION OF OUTPUT
! ---------------------
! This simple CCF processor creates a file that consists only of the lines
! that the Fortran processor would see.  In other words, all CCF lines
! (those with !ccf$ in columns 1 to 5) will not appear in the created file
! and all lines that are in a FALSE branch of a CCF if-construct will not
! appear in the created file.
!
! DIFFERENCES BETWEEN THIS CCF AND FULL CCF
! -----------------------------------------
! This simple CCF processor differs from the CCF in your Fortran processor
! in a few ways:
!   1) Two files must be maintained because this CCF processor does not
!      give the option of creating a file with the same number of lines
!      as the input file.  A full CCF processor can comment lines in the
!      the FALSE branch of a CCF if-construct with a !ccf* or !ccf>.  This
!      CCF processor will not include CCF lines or the lines in a FALSE
!      branch of a CCF if-construct.
!   2) This CCF processor creates two temp files that must be compiled and
!      run in order to create the CCF output file.  One of these temp files
!      is INCLUDEd in the other temp file.  A full CCF processor does not
!      require creating extra files.
!   3) The CCF PRINT and STOP statements are executed at compile time in
!      the CCF in your Fortran processor.  In this CCF processor the CCF
!      PRINT and STOP statements are executed when the ccf temp file is
!      executed.  This slightly hinders the usability of these CCF stmts.
!   4) There is a limit on the number of CCF variable declaration lines
!      (you may change this limit by changing MAX_CCF_VAR_DECL_LINES).
!   5) Fortran keywords INTEGER and LOGICAL are reserved words for this
!      CCF processor (do not name a CCF variable "integer" or "logical").
!   6) Diagnostic checks are not made on the CCF statements.  Please stick
!      to the CCF language definition (CCF variables do not have "kinds",
!      CCF statements do not have labels, use free source form for the
!      CCF lines (even if your source is fixed source form), etc.).
!*******************************************************************************

program SimpleCcf
implicit NONE

  ! for buffering CCF variable declarations so they can be written to a MODULE
  integer, parameter    :: MAX_CCF_VAR_DECL_LINES = 10
  character (len = 132) :: ccf_var_lines(MAX_CCF_VAR_DECL_LINES)
  integer               :: num_ccf_var_decl_lines = 0

  ! for storing lines of input
  character (len = 132) :: line

  ! file names (limit of 32 is a random selection and can be changed)
  character (len = 132)  :: in_file, &     ! Input file to be CCFed
                            out_file       ! Output file excludes CCF lines and
                                          ! lines inside FALSE branches
  character (len = 132)  :: temp_file1, &  ! Middle-step file MAIN
                            temp_file2,  & ! Middle-step file MODULE
                            batch_file     ! Option to run CCF batch mode

  ! batch mode for input (input and output filenames and initial values)
  logical               :: batch          ! TRUE if batch_file exists

!CCF$ integer :: dos = 1
!CCF$ integer :: unix = 2
!CCF$ integer :: your_system = 3 ! Edit here for system other than dos or unix
!CCF$ integer :: system = 2

   ! set filenames
!CCF$ if (system == dos .OR. system == unix) then
   temp_file1 = "ccftemp.f90"
   temp_file2 = "ccfmod.f90"
   batch_file = "ccfbatch.dat"
!CCF$ else ! system other than dos or unix
!CCF$    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!CCF$    stop 'Please edit here and above for system other than dos or unix'
!CCF$    !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!CCF>   temp_file1 = "ccftemp filename for your system"
!CCF>   temp_file2 = "ccfmod filename for your system"
!CCF>   batch_file = "ccfbatch filename for your system"
!CCF$ endif

   inquire (FILE=batch_file, EXIST=batch)
   if (batch) then
      open (UNIT=7, ACTION="READ", &
            STATUS="OLD", POSITION="REWIND", ERR=101, FILE=batch_file)
      read (UNIT=7, FMT="(A)", END=102) in_file
      read (UNIT=7, FMT="(A)", END=102) out_file
   else
      print *, "Enter the name of the file to be CCFed:"
      read *, in_file
      print *, "Enter the name you want for the output file:"
      read *, out_file
   endif

   open (UNIT=8, ACTION="READ", &
         STATUS="OLD", POSITION="REWIND", ERR=103, FILE=in_file)
   open (UNIT=9, ACTION="WRITE", &
         STATUS="REPLACE", POSITION="REWIND", ERR=104, FILE=temp_file1)

   call WriteLine(9, "INCLUDE '" // TRIM(temp_file2) // "'"                   )
   call WriteLine(9, "PROGRAM CcfIt"                                          )
   call WriteLine(9, "USE CcfVars    ! Module with CCF vars and init values"  )
   call WriteLine(9, "implicit NONE"                                          )
   call WriteLine(9, " "                                                      )
   call WriteLine(9, "CALL mCcfInits  ! Init CCF values"                      )
   call WriteLine(9, 'open (UNIT=9, ACTION="WRITE", STATUS="REPLACE", ERR=7,&')
   call WriteLine(9, '      POSITION="REWIND", FILE="' //TRIM(out_file)// '")')

   !**************************************************************************!
   !**                                                                      **!
   !**  Read each line of the input file looking for                        **!
   !**    1) CCF lines                                                      **!
   !**    2) lines previously commented out by CCF in a Fortran processor   **!
   !**    3) other lines                                                    **!
   !**                                                                      **!
   !**    1) CCF lines -                                                    **!
   !**        a) CCF variable declarations are buffered until written to    **!
   !**           the CCF module                                             **!
   !**        b) all other CCF lines are turned into Fortran lines by       **!
   !**           replacing the !ccf$ in columns 1-5 with blanks             **!
   !**                                                                      **!
   !**    2) lines previously commented out by CCF in a Fortran processor - **!
   !**        a) !ccf* in columns 1-5 - Replaced columns 1-5 with blanks    **!
   !**        b) !ccf> in columns 1-5 - Shift the line left 5 columns       **!
   !**                                                                      **!
   !**        Double all double quotes (") as in 3)                         **!
   !**                                                                      **!
   !**    3) other lines -                                                  **!
   !**        Other lines are simply echoed to the output file.  Doing      **!
   !**        this requires doubling all the double quotes (").             **!
   !**                                                                      **!
   !**************************************************************************!
   do ! until end of file
      read (UNIT=8, FMT="(A)", END=50) line

      ! !ccf$ - a ccf line
      if ((line(1:5) == '!ccf$') .OR. (line(1:5) == '!CCF$')) then

         ! if a CCF var declaration, save this line for the CcfVar MODULE
         ! else replace the !ccf$ with blanks (turn into a Fortran statement)
         if (CcfVarDeclaration()) then
            num_ccf_var_decl_lines = num_ccf_var_decl_lines + 1
            if (num_ccf_var_decl_lines > MAX_CCF_VAR_DECL_LINES) then
               print *, 'CCF halts.  Limit of CCF variable declaration lines'
               print *, MAX_CCF_VAR_DECL_LINES,'exceeded on line:'
               print *, line
               stop
            endif
            line(1:5) = '     ' ! replace !ccf$ with blanks
            ccf_var_lines(num_ccf_var_decl_lines) = line
         else
            line(1:5) = '     ' ! replace !ccf$ with blanks
            call WriteLine(9, line)
         endif
      else ! not a CCF line
         ! !ccf* - a non-ccf line, commented by ccf by replacing columns 1 to 5
         ! !ccf> - a non-ccf line, commented by ccf by shifting the line
         !         right 5 columns and placing !ccf> in columns 1-5
         if ((line(1:5) == '!ccf*') .OR. (line(1:5) == '!CCF*')) then
            line(1:5) = '     '  ! blank out "!ccf$"
         elseif ((line(1:5) == '!ccf>') .OR. (line(1:5) == '!CCF>')) then
            line = line(6:80)    ! shift left 5 char
         else
            ! No !ccf* or !ccf> to blank out or shift.
         endif

         ! Double each occurrence of a double quote (") and output the new line
         call HandleDoubleQuotesThenWriteLine()
      endif
   enddo
   50 continue  ! end of input file

   call WriteLine (9, "goto 8 ! We are done."                               )
   call WriteLine (9, "7 print *, 'Trying to open CCF output file with the&")
   call WriteLine (9, "& name you supplied >" // TRIM(out_file) // "'"      )
   call WriteLine (9, "stop 'CCF Error: opening this output file.'"         )
   call WriteLine (9, "8 continue"                                          )
   call WriteLine (9, "END")
   call WriteLine (9, " ")
   call WriteLine (9, "subroutine WriteLine(unit_num, line)           ")
   call WriteLine (9, "   integer :: unit_num                         ")
   call WriteLine (9, "   character (len=*) :: line                   ")
   call WriteLine (9, "                                               ")
   call WriteLine (9, '   write (UNIT=unit_num, FMT="(A)") TRIM(line) ')
   call WriteLine (9, "end subroutine WriteLine                       ")

   ! Write the MODULE file which contains all the CCF variable declarations
   ! and any initial values.
   call WriteCcfVarsModule()

   ! Reminder of middle-step
   if (.NOT. batch) then
!      print *
!      print *, "CCF is creating ", TRIM(temp_file1), &
!               " as the temporary file to compile and run"
   endif

   ! Close files and we are done
   close (UNIT=7, STATUS="KEEP", ERR=105)
   close (UNIT=8, STATUS="KEEP", ERR=106)
   close (UNIT=9, STATUS="KEEP", ERR=107)
   goto 110

   !**** ERROR messages ******************************************************!
   101  print *, 'Trying to open CCF batch file "', TRIM(batch_file), '"'
   stop 'CCF Error: opening this input file.'
   102  print *, 'Trying to read a line from CCF Batch file "', &
                 TRIM(batch_file), '"'
   stop 'CCF Error: Batch file expecting input and output filenames'
   103  print *, 'Trying to open CCF input file "', TRIM(in_file), '"'
   stop 'CCF Error: opening this input file.'
   104  print *, 'Trying to open CCF temp file "', TRIM(temp_file1), '"'
   stop 'CCF Error: opening this input/output file.'
   105  print *, 'Trying to close CCF batch file "', TRIM(batch_file), '"'
   stop 'CCF Error: closing this input file.'
   106  print *, 'Trying to close CCF input file "', TRIM(in_file), '"'
   stop 'CCF Error: closing this input file.'
   107  print *, 'Trying to close CCF temp file "', TRIM(temp_file1), '"'
   stop 'CCF Error: closing this input/output file.'
   ! end ERROR messages ******************************************************!

   110 continue  ! no I/O errors.  We are done.

contains

!*******************************************************************************

subroutine HandleDoubleQuotesThenWriteLine()

! Echo a non-CCF line to the middle-step temp file.  This is done by
! turning the line into a character constant.  Turning a line into a
! character constant requires placing it in between two double quotes (")
! and doubling each occurrence of a double quote.  The new line is then
! passed to a WriteLine subroutine.  Note that the original line could
! be 132 double quotes.

  character (len = 264) :: did_quotes_line ! line after doubling the "
  integer               :: new_len         ! line len after doubling the "
  character (len = 285) :: new_line        ! 285 handles a line of 132 "s as:
                                           !   1-19 : call WriteLine(9, "
                                           !  20-283: 264 "s (132 "s doubled)
                                           ! 284-285: ")
  integer :: pos,     &  ! pos in original line
             new_pos     ! position in did_quotes_line

   did_quotes_line = ' '
   new_pos = 1

   ! Double each occurrence of a double quote (").
   do pos = 1, LEN_TRIM(line)
      if (line(pos:pos) == '"') then
         did_quotes_line(new_pos:new_pos+1) = '""'
         new_pos = new_pos + 2
      else
         did_quotes_line(new_pos:new_pos) = line(pos:pos)
         new_pos = new_pos + 1
      endif
   enddo
   new_len = new_pos

   new_line(1:19) = 'call WriteLine(9, "'
   new_line(20:20+new_len)              = did_quotes_line(1:new_len)
   new_line(20+new_len+1:20+new_len+2)                             = '")'

   ! Set new_len to the length of this Fortran statement that may need splitting
   new_len = 20+new_len+2
   if (new_len <=132) then
      new_line(new_len+1:132) = ' '
      call WriteLine(9, new_line(1:132))
   elseif (new_len <= 262) then
   ! need to split line once
      call WriteLine(9, new_line(1:131)//"&")
      call WriteLine(9, "&"//new_line(132:262))
   else
   ! need to split line twice
      call WriteLine(9, new_line(1:131)//"&")
      call WriteLine(9, "&"//new_line(132:261)//"&")
      call WriteLine(9, "&"//new_line(262:new_len))
   endif

end subroutine HandleDoubleQuotesThenWriteLine

!*******************************************************************************

function CcfVarDeclaration()

! Determine if the current CCF line ("line") is a CCF variable declaration
! line.  This simple CCF processor expects free source form, keywords are
! reserved words, and there are no "kinds" on these variables.

  logical              :: CcfVarDeclaration ! TRUE if CCF INTEGER or
                                            !         CCF LOGICAL statement
  integer              :: pos               ! pos in line

   ! skip over the !ccf$ and the blanks
   pos = 6
   do while (line(pos:pos) == ' ')
      pos = pos + 1
   end do

   ! This Simple CCF processor expects a blank or a ':' after
   ! INTEGER or LOGICAL.  Note that INTEGER and LOGICAL are reserved words.
   if (((line(pos:pos+6) == 'INTEGER') .OR. &
        (line(pos:pos+6) == 'LOGICAL') .OR. &
        (line(pos:pos+6) == 'integer') .OR. &
        (line(pos:pos+6) == 'logical'))     &
       .AND.                                &
       ((line(pos+7:pos+7) == ' ') .OR.     &
        (line(pos+7:pos+7) == ':'))) THEN
      CcfVarDeclaration = .TRUE.
   else
      CcfVarDeclaration = .FALSE.
   endif
end function CcfVarDeclaration

!*******************************************************************************

subroutine WriteCcfVarsModule()

! Write the CcfVars MODULE.  CcfVars contains all the CCF variable
! declarations and a subroutine called mCcfInits.  mCcfInits contains
! assignments of any initial values for CCF variables supplied either
! in the CCF batch file or from standard input.

  integer              :: i            ! counter for loop
  character (len = 32) :: init_var, &  ! CCF variable to be initialized
                          init_val     ! intial value for CCF variable

   open (UNIT=10, ACTION="WRITE", STATUS="REPLACE", &
         POSITION="REWIND", ERR=201, FILE=temp_file2)

   call WriteLine(10, "MODULE CcfVars")
   call WriteLine(10, "implicit NONE")

   do i = 1, num_ccf_var_decl_lines
      call WriteLine(10, ccf_var_lines(i))
   enddo

   call WriteLine(10, "CONTAINS")
   call WriteLine(10, "SUBROUTINE mCcfInits")

   init_var = "foo" ! any foo other than '0' since '0' terminates loop
   do while (init_var /= '0')
      if (batch) then
         read (UNIT=7, FMT="(A)", END=202) init_var
         if (init_var(1:1) /= '0') then
            read (UNIT=7, FMT="(A)", END=202) init_val
            line = init_var // "=" // init_val
            call WriteLine(10, line)
         endif
      else
!         print *, "Enter the name of a CCF variable to be initialized"
!         print *, "(or '0' when you are done initializing):"
         read *, init_var
         if (init_var /= '0') then
            print *, "Enter the value you want for this CCF variable: "
            read *, init_val
            line = init_var // "=" // init_val
            call WriteLine(10, line)
         endif
      endif
   enddo

   call WriteLine(10, "END SUBROUTINE mCcfInits")
   call WriteLine(10, "END MODULE CcfVars")

   close (UNIT=10, STATUS="KEEP", ERR=203)
   goto 210

   !**** ERROR messages ******************************************************!
   201  print *, 'Trying to open CcfVars Module file "', TRIM(temp_file2), '"'
   stop 'CCF Error: opening this output file.'
   202  print *, 'Trying to read a line from CCF Batch file "', &
                 TRIM(batch_file), '"'
   stop 'CCF Error: Batch file expecting initial var-val pairs or 0'
   203  print *, 'Trying to close CcfVars Module file "', TRIM(temp_file2), '"'
   stop 'CCF Error: closing this output file.'
   ! end ERROR messages ******************************************************!

   210 continue ! no I/O errors
end subroutine WriteCcfVarsModule

end program
!*******************************************************************************

subroutine WriteLine(unit_num, line)
implicit NONE
  integer :: unit_num
  character (len=*) :: line

   write (UNIT=unit_num, FMT="(A)") TRIM(line)
end subroutine WriteLine

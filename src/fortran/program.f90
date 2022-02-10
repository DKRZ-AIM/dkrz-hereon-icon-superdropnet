
PROGRAM routine
  USE mo_type, ONLY: t_scalar_field_1d
  ! the main program
  ! call subroutines from here 

  ! sketch
  ! - define a field with variables
  ! - define routines to get / set these variables
  ! - define a function that operates on the variables /
  !   calculates something based on the variables
  ! - later replace this function by a python routine
  
  TYPE(t_scalar_field_1d) :: scalar_field_1d

  scalar_field_1d% k = 10

END PROGRAM routine

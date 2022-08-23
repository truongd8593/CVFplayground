!  TestCVF.f90 
!
!  FUNCTIONS:
!	TestCVF      - Entry point of console application.
!
!	
!

!****************************************************************************
!
!  PROGRAM: TestCVF
!
!  PURPOSE: Test Compaq Visual Fortran with derived type, pointer etc.
!
!****************************************************************************

	program TestCVF

	use mdl_linked_list
	implicit none
	integer           :: i
	type(linked_list) :: list
	
	do i = 1, 10
		call prepend(list, i)
	enddo
	call display(list)

	end program TestCVF


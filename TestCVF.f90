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

	implicit none

	write(*, *) '============================================================'
	write(*, *) 'Test linked list:'
	write(*, *) '============================================================'
	call test_linked_list
	write(*, *) 'End of test linked list'
	write(*, *) '============================================================'
	write(*, *) 'Test stack:'
	call test_stack
	write(*, *) 'End of test stack'
	write(*, *) '============================================================'


	end program TestCVF


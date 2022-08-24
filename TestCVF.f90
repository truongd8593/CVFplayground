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
	integer           :: i, key
	type(linked_list) :: l1, l2
	
	do i = 1, 10
		call prepend(l1, i)
	enddo
	print*, 'List 1:'
	call display(l1)

	do i = 1, 10
		call append(l2, i)
	enddo
	print*, 'List 2:'
	call display(l2)

	key = 3
	print*, 'Delete node has key = ', key
	call remove_node(l2, key)
	call display(l2)

	key = 10
	print*, 'Delete node has key = ', key
	call remove_node(l2, key)
	call display(l2)

	end program TestCVF


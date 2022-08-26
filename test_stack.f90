subroutine test_stack
	use mdl_stack
	implicit none

	type(stack) :: s
	integer     :: i

	do i = 1, 10
		call push(s, i)
	enddo
	write(*, *) 'Stack:'
	call display(s)
	write(*, *) 'Delete head of stack ...'
	call pop(s)
	call display(s)
end subroutine
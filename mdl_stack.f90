module mdl_stack
	use mdl_linked_list, only: stack => linked_list, &
	                                    prepend, display, &
	                     remove_head => remove_head_node
	implicit none

	contains

		logical function is_empty(s)
			implicit none
			type(stack) :: s

			if (.not. associated(s%head)) then
				is_empty = .true.
			else
				is_empty = .false.
			endif
		end function

		subroutine push(s, num)
			implicit none
			type(stack) :: s
			integer     :: num

			call prepend(s, num)

		end subroutine

		subroutine pop(s)
			implicit none
			type(stack) :: s

			if (is_empty(s)) return
			call remove_head(s)
		end subroutine
end module
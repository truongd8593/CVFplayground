module mdl_linked_list
	implicit none

	type :: node
		integer             :: value
		type(node), pointer :: next => null()
	end type node

	type :: linked_list
		type(node), pointer :: head => null()
		type(node), pointer :: tail => null()
	end type linked_list

	contains
		subroutine prepend(list, num)
			implicit none
			type(linked_list)   :: list
			integer             :: num
			type(node), pointer :: current => null()

			if (.not. associated(list%head)) then
				allocate(list%head)
				list%head%value = num
				list%tail => list%head
			else
				allocate(current)
				current%value = num
				current%next => list%head
				list%head => current
			endif
			
		end subroutine

		subroutine append(list, num)
			implicit none
			type(linked_list)   :: list
			integer             :: num
			type(node), pointer :: current => null()

			if (.not. associated(list%head)) then
				allocate(list%head)
				list%head%value = num
				list%tail => list%head
			else
				allocate(current)
				current%value = num
				list%tail%next => current
				list%tail => current
			endif
			
		end subroutine

		subroutine display(list)
			implicit none
			type(linked_list)   :: list
			type(node), pointer :: current

			print *, '['
			current => list%head
			do
				if (.not. associated(current)) exit
				print *, current%value, ','
				current => current%next
			enddo
			print *, ']'

		end subroutine
end module mdl_linked_list
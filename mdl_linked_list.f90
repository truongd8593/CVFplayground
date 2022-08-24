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
				call allocate_node(list%head, num)
				list%tail => list%head
			else
				call allocate_node(current, num)
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
				call allocate_node(list%head, num)
				list%tail => list%head
			else
				call allocate_node(current, num)
				list%tail%next => current
				list%tail => current
			endif
			
		end subroutine

		subroutine display(list)
			implicit none
			type(linked_list)   :: list
			type(node), pointer :: current

			write(*, '(a)', advance = 'no') '['
			current => list%head
			do
				if (.not. associated(current)) exit
				if (.not. associated(current%next)) then
					write(*, '(1x, i0, a)', advance = 'no') current%value, ']'
				else
					write(*, '(1x, i0, a)', advance = 'no') current%value, ','
				endif
				current => current%next
			enddo
			write(*, *)

		end subroutine

		subroutine allocate_node(new_ele, datum)
			implicit none
			type(node), pointer :: new_ele
			integer             :: datum

			allocate(new_ele)
			new_ele%value = datum
			nullify(new_ele%next)

		end subroutine

		function search_node(list, key)
			implicit none
			type(linked_list)   :: list
			type(node), pointer :: p
			type(node), pointer :: search_node
			integer             :: key

			p => list%head
			do while (associated(p))
				if (p%value == key) exit
				p => p%next
			enddo
			search_node => p
		end function

		function search_prev_node(list, key)
			implicit none
			type(linked_list)   :: list
			type(node), pointer :: p
			type(node), pointer :: search_prev_node
			type(node), pointer :: q => null()
			integer             :: key

			p => list%head
			do while (associated(p))
				if (p%value == key) exit
				q => p
				p => p%next
			enddo
			search_prev_node => q

		end function

		subroutine remove_node(list, key)
			implicit none
			type(linked_list)   :: list
			integer             :: key
			type(node), pointer :: p => null()
			type(node), pointer :: q => null()

			p => search_node(list, key)
			q => search_prev_node(list, key)
			
			if (.not. associated(p)) then
				write(*, *) 'Cannot find key ', key
				return
			endif

			if (associated(q)) then
				if (associated(p, list%tail)) list%tail => q
				q%next => p%next
				nullify(p)
				deallocate(p)
			else
				list%head => p%next
				if (.not. associated(list%head)) nullify(list%tail)
			endif

		end subroutine


end module mdl_linked_list
SUBROUTINE Test_Gen_Ll

! Defines and manipulates list(s) of a user-defined type all based on
! a single generic list type. 

USE Generic_List, ONLY : Link_Ptr_Type,&
                         Link_Type,&
						 List_Type
USE Generic_List, ONLY : LI_Init_List,LI_Add_To_Head,LI_Get_Head,&
                         LI_Remove_Head,LI_Get_Next,LI_Associated,&
						 LI_Get_Len,LI_Flip_Direction,LI_Add_To_Tail

IMPLICIT NONE

! User-defined list element
! The Link_Type field MUST be the FIRST in the user-defined list element 
! Note pointer to data so as to easily create sublists
TYPE User_Type
  TYPE(Link_Type) :: Link
  TYPE(User_Data_Type), POINTER :: Data  
END TYPE User_Type

TYPE User_Data_Type
  INTEGER :: Index
  INTEGER :: User_Stuff
END TYPE User_Data_Type

! Auxilliary data type required for the transfer function
TYPE User_Ptr_Type
  TYPE(User_Type), POINTER :: P
END TYPE User_Ptr_Type

TYPE(List_Type)      :: User_List,Sublist,User_List_2
TYPE(Link_Ptr_Type)  :: Link,Sublink
TYPE(User_Ptr_Type)  :: User,Subuser

INTEGER I,N


! Initialize list
CALL LI_Init_List(User_List)

! Build up list (add to stack)
N=5
DO I=1,N
   ALLOCATE(User%P); ALLOCATE(User%P%Data)
   User%P%Data%Index = I
   User%P%Data%User_Stuff = I*I
   Link = TRANSFER(User,Link)
   CALL LI_Add_To_Head(Link,User_List)
ENDDO
CALL LI_Flip_Direction(User_List)

! Cycle through list
Link = LI_Get_Head(User_List)
DO WHILE(LI_Associated(Link))
   User = TRANSFER(Link,User)
   WRITE(6,*)User%P%Data%Index,User%P%Data%User_Stuff
   Link = LI_Get_Next(Link)
ENDDO

! Find list length
WRITE(6,*)
WRITE(6,*)'Length = ',LI_Get_Len(User_List)
WRITE(6,*)

! Make a sublist
CALL LI_Init_List(Sublist)
Link = LI_Get_Head(User_List)
DO WHILE(LI_Associated(Link)) 
   User = TRANSFER(Link,User)
   IF(User%P%Data%Index.NE.4)THEN
   ALLOCATE(Subuser%P)
   Subuser%P%Data => User%P%Data	
   Sublink = TRANSFER(Subuser,Sublink)
   CALL LI_Add_To_Head(Sublink,Sublist)
   ENDIF
   Link = LI_Get_Next(Link)
ENDDO
! Cycle through sublist
Sublink = LI_Get_Head(Sublist)
DO WHILE(LI_Associated(Sublink))
   Subuser = TRANSFER(Sublink,Subuser)
   WRITE(6,*)Subuser%P%Data%Index,Subuser%P%Data%User_Stuff
   Sublink = LI_Get_Next(Sublink)
ENDDO
! Find sublist length
WRITE(6,*)
WRITE(6,*)'Sublength = ',LI_Get_Len(Sublist)
WRITE(6,*)

! Remove from list (stack)
WRITE(6,*)
DO
   Link = LI_Remove_Head(User_List)
   IF(.NOT.LI_Associated(Link))EXIT
   User = TRANSFER(Link,User)
   WRITE(6,*)User%P%Data%Index,User%P%Data%User_Stuff
   DEALLOCATE(User%P)
ENDDO

! Find list length
WRITE(6,*)
WRITE(6,*)'Length = ',LI_Get_Len(User_List)
WRITE(6,*)

! Build up list (add to stack)
N=5
DO I=1,N
   ALLOCATE(User%P); ALLOCATE(User%P%Data)
   User%P%Data%Index = I
   User%P%Data%User_Stuff = I*I
   Link = TRANSFER(User,Link)
   CALL LI_Add_To_Tail(Link,User_List_2)
ENDDO

! Cycle through list
Link = LI_Get_Head(User_List_2)
DO WHILE(LI_Associated(Link))
   User = TRANSFER(Link,User)
   WRITE(6,*)User%P%Data%Index,User%P%Data%User_Stuff
   Link = LI_Get_Next(Link)
ENDDO

! Remove from list (stack)
WRITE(6,*)
DO
   Link = LI_Remove_Head(User_List_2)
   IF(.NOT.LI_Associated(Link))EXIT
   User = TRANSFER(Link,User)
   DEALLOCATE(User%P)
ENDDO

END SUBROUTINE

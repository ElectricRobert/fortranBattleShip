!********************************************************************
!
!                   Battleship Fortran
!                  created by: Ya boy ryan
!
!********************************************************************



!*************************  Main Program ****************************
program grid 

implicit none
integer :: grid_Status_mask(0:9,0:9)
integer :: row,col,k,x,y,winCount = 0
integer*4 i,n
integer :: seed
character :: player_grid(0:9,0:9)

! *** Ship Characteristics ***
integer,parameter :: airCraftCarrier = 5
integer,parameter :: battleShip = 4
integer,parameter :: submarine = 3
integer,parameter :: destroyer = 2
integer,parameter :: patrolBoat = 1

! ******  Number of each boat  *******
integer,parameter :: numACC = 1
integer,parameter :: numbattle = 2
integer,parameter :: numsub = 8
integer,parameter :: numdest = 8
integer,parameter :: numpatrol = 8
! *** grid symbol characteristics ***
character,parameter :: hit = 'X'
character,parameter :: miss = 'O'



call intro

call initialize_grid_mask(grid_Status_mask) ! intialize background grid
call print_grid_mask(grid_Status_mask)

call initialize_player_grid(player_grid) ! intialize player grid
call print_player_grid(player_grid)

! *** assign random ships ***

do k = 1,numACC

call assignShips(grid_Status_mask,airCraftCarrier)

end do

do k = 1,numbattle

call assignShips(grid_Status_mask,battleShip)

end do

do k = 1,numsub

call assignShips(grid_Status_mask,submarine)

end do

do k = 1,numdest

call assignShips(grid_Status_mask,destroyer)

end do

do k = 1,numPatrol

call assignShips(grid_Status_mask,patrolBoat)

end do




call print_grid_mask(grid_Status_mask)

do x = 0,9
  do y = 0,9
    if(grid_Status_mask(x,y) /= 0) then
      winCount = winCount + 1
    end if
  end do
end do


end program ! end main program

!************************ End Main Program ****************************










!********************************************************************
!                             Introduction
!********************************************************************

! ***  intro function introduces main functionality of game ****
subroutine intro 

print*,'Welcome to BattleShip'

return
end subroutine


!********************************************************************
!                  Grid Mask Status Functions
!********************************************************************



! ***  initilizes beginning status of game board ****
subroutine initialize_grid_mask(grid_Status_mask)
implicit none
integer, intent(out) :: grid_Status_mask(0:9,0:9)
integer :: row,col

do row = 0,9
   do col = 0,9
    grid_Status_mask(row,col) = 0
   end do ! end col
end do ! end row

end subroutine



! *** prints status mask ****
subroutine print_grid_mask(grid_Status_mask)
implicit none
integer, intent(out) :: grid_Status_mask(0:9,0:9)
integer :: row,col

write(*,*)! prints new line
do row = 0,9
   do col = 0,9

       write(*, '(I2,X)', advance='no') grid_Status_mask(row,col)

   end do ! end col
   
   write(*,*) ! prints new line

end do ! end row

end subroutine




!********************************************************************
!                  Player Grid Status Functions
!********************************************************************


! ***  initilizes beginning status of player grid ****
subroutine initialize_player_grid(player_grid)
implicit none
character, intent(out) :: player_grid(0:9,0:9)
integer :: row,col


do row = 0,9
   do col = 0,9
    player_grid(row,col) = '~'
   end do ! end col
end do ! end row

end subroutine


! *** prints player grid ****
subroutine print_player_grid(player_grid)
implicit none
character, intent(out) :: player_grid(0:9,0:9)
integer :: row,col
write(*,*)! prints new line
do row = 0,9
   do col = 0,9

       write(*, '(A1,X)', advance='no') player_grid(row,col)

   end do ! end col
   
   write(*,*) ! prints new line

end do ! end row

end subroutine


!********************************************************************
!                     Ship Assignment Functions
!********************************************************************
subroutine assignShips(grid_Status_mask,ship)
 
 integer, intent(out) :: grid_Status_mask(0:9,0:9)
 integer, intent(in) :: ship
 integer :: r_row,r_col ! random row and col
 integer :: orientation ! choose random ship orientation
 integer :: i,j,k,newIndex!index 
 integer :: boundFlag,intersectFlag ! flag used to check bounds & intersection
 integer:: aCCCount, battleCount, subCount, destCount, patrolCount !number of each ship
 real:: x
integer :: iseed = 0

 boundFlag = 0
 intersectFlag = 0 ! initialize flags


 aCCCount = 0
 battleCount = 0
 subCount = 0
 destCount = 0
 patrolCount = 0  ! initilize ships

 

 ! ##############################
 !     Ship Orientations
 !    ___________________
 !     right -> 1
 !     left  -> 2
 !     up    -> 3
 !     down  -> 4
 !
 ! ##############################


 ! For each ship type choose starting coordinate and orientation

 i = ship
    if(i > 5) then
    return
    end if
    do while(boundFlag /= 1 .AND. intersectFlag /= 1)
   
     r_row = rand()*10
     r_col = rand()*10   ! Choose random coordinate and orientation
    
     orientation = (rand()*4)+1

 ! *************************************************************************
 !               Bounds Checking & Intersection Checking
 !          - For a given ship size and randomized coordinate and 
 !         orientation, the ships bounds are checked against each of the grid's 
 !         respective maximum and minimum array bounds.
 !        
 !         - If either the bounds are too small compared to the ship's size
 !         or the an adjacent scan of the given orientation detects any ship
 !         another coordinate and orientation is randomized until parameters
 !         are met.
 ! *************************************************************************
 ! #########################################################################
 !                           Check Right
 ! #########################################################################
     if (orientation == 1) then ! right
        
	   if(r_col + i - 1 > 9 ) then ! check right bounds
	  ! if greater than 9, re-choose starting coordinate until bounds met
	    boundFlag = 0
	    r_row = rand()*10
 	    r_col = rand()*10
 	    orientation = (rand()*4)+1

 	  else 
                boundFlag = 1
	  end if ! end bounds check

          do j = r_col,r_col + i - 1 ! adjacency check
    
		  if(grid_Status_mask(r_row,j) /= 0) then !if any adjacent cell is not zero
		    intersectFlag = 0
		    r_row = rand()*10
	 	    r_col = rand()*10
	 	    orientation = (rand()*4)+1
		    exit
		  else
		   intersectFlag = 1
		  end if 
          end do ! end adjacency check
          
 ! #########################################################################
 !                           Check Left
 ! #########################################################################
     else if (orientation == 2) then ! left


            if(r_col - i - 1 < 0 ) then ! check right bounds
	  ! if greater than 9, re-choose starting coordinate until bounds met
	    boundFlag = 0
	    r_row = rand()*10
 	    r_col = rand()*10
 	    orientation = (rand()*4)+1

 	  else 
                boundFlag = 1
	  end if ! end bounds check

          do j = r_col,r_col - i - 1 ! adjacency check
    
		  if(grid_Status_mask(r_row,j) /= 0) then !if any adjacent cell is not zero
		    intersectFlag = 0
		    r_row = rand()*10
	 	    r_col = rand()*10
	 	    orientation = (rand()*4)+1
		    exit
		  else
		   intersectFlag = 1
		  end if 
          end do ! end adjacency check



 ! #########################################################################
 !                           Check Up
 ! #########################################################################
     else if (orientation == 3) then ! up

           if(r_row - i - 1 < 0 ) then ! check right bounds
	  ! if greater than 9, re-choose starting coordinate until bounds met
	    boundFlag = 0
	    r_row = rand()*10
 	    r_col = rand()*10
 	    orientation = (rand()*4)+1

 	  else 
                boundFlag = 1
	  end if ! end bounds check

          do j = r_row,r_row - i - 1 ! adjacency check
    
		  if(grid_Status_mask(j,r_col) /= 0) then !if any adjacent cell is not zero
		    intersectFlag = 0
		    r_row = rand()*10
	 	    r_col = rand()*10
	 	    orientation = (rand()*4)+1
		    exit
		  else
		   intersectFlag = 1
		  end if 
          end do ! end adjacency check



 ! #########################################################################
 !                           Check down
 ! #########################################################################
     else if (orientation == 4) then ! down

          if(r_row + i - 1 < 0 ) then ! check right bounds
	  ! if greater than 9, re-choose starting coordinate until bounds met
	    boundFlag = 0
	    r_row = rand()*10
 	    r_col = rand()*10
 	    orientation = (rand()*4)+1

 	  else 
                boundFlag = 1
	  end if ! end bounds check

          do j = r_row,r_row + i - 1 ! adjacency check
    
		  if(grid_Status_mask(j,r_col) /= 0) then !if any adjacent cell is not zero
		    intersectFlag = 0
		    r_row = rand()*10
	 	    r_col = rand()*10
	 	    orientation = (rand()*4)+1
		    exit
		  else
		   intersectFlag = 1
		  end if 
          end do ! end adjacency check      



    else 
     
    write(*,*)'Error!'

    end if ! end orientation


    end do ! end flag

 ! #########################################################################
 !                      Assign Ship to grid
 ! #########################################################################

    if(boundFlag == 1 .AND. intersectFlag == 1) then


       if(orientation == 1) then ! right

	     do j = r_col,r_col + i - 1 ! assign right direction
	         grid_Status_mask(r_row,j) = i
	     end do 
 
       else if (orientation == 2 ) then ! left

             do j = r_col,r_col - i - 1 ! assign left direction
	         grid_Status_mask(r_row,j) = i
	     end do 

       else if (orientation == 3) then ! up
            
             do j = r_row,r_row - i - 1 ! assign up direction
	         grid_Status_mask(j,r_col) = i
	     end do 

       else if (orientation == 4) then ! down
		
	    do j = r_row,r_row + i - 1 ! assign down direction
	         grid_Status_mask(j,r_col) = i
	    end do 

       end if

     boundFlag = 0
     intersectFlag = 0  ! reset flags for next iteration

    end if

end subroutine !end shipPlacement


 ! #########################################################################
 !                      Determine maximum hit  count
 ! #########################################################################




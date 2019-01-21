%part1 22.12.2018
%In the graph below you see the possible flights between some of the cities in Turkey.

% Hamza Yoğurtcuoğlu
% 171044086

%if conflicts is returning true.There is no conflict or yes.
%All control is perfectly work just 4.control direclty not true answer 
%

%conflicts(CourseID1,CourseID2)
%assign(RoomID,CourseID)
%assign(RoomID)
%enroll(StudentID,CourseID)
%enroll(StudentID)

%    ID  Capacity      Equipment

room(z06,10,projector).
room(z06,10,hcapped).

room(z11,11,hcapped).
room(z11,11,smartboard).

%    ID  Instructor  Capacity   Hour   Needs
course(cse341,genc,10,4,z06).
course(cse343,turker,6,3,z06).
course(cse331,bayrakci,5,3,z06).
course(cse321,gozupek,10,10,z11).

%    ID    Hour   Course
occupancy(z06,8,cse341).
occupancy(z06,9,cse341).
occupancy(z06,10,cse341).
occupancy(z06,11,cse341).
occupancy(z06,12,_).
occupancy(z06,13,cse331).
occupancy(z06,14,cse331).
occupancy(z06,15,cse331).
occupancy(z06,16,_).

occupancy(z11,8,cse343).
occupancy(z11,9,cse343).
occupancy(z11,10,cse343).
occupancy(z11,11,cse343).
occupancy(z11,12,_).
occupancy(z11,13,_).
occupancy(z11,14,cse321).
occupancy(z11,15,cse321).
occupancy(z11,16,cse321).


%     SID      Courses    Hcapped  
student(1,cse341,no).
student(1,cse343,no).
student(1,cse331,no).

student(2,cse341,no).
student(2,cse343,no).

student(3,cse341,no).
student(3,cse331,no).

student(4,cse341,no).

student(5,cse331,no).
student(5,cse343,no).

student(6,cse341,yes).
student(6,cse343,yes).
student(6,cse331,yes).

student(7,cse341,no).
student(7,cse343,no).

student(8,cse341,yes).
student(8,cse331,yes).

student(9,cse341,no).

student(10,cse341,no).
student(10,cse321,no).

student(11,cse341,no).
student(11,cse321,no).

student(12,cse343,no).
student(12,cse321,no).

student(13,cse343,no).
student(13,cse321,no).

student(14,cse343,no).
student(14,cse321,no).

student(15,cse343,yes).
student(15,cse321,yes).


%     ID    Courses   Needs
instructor(genc,cse341,projector).
instructor(turker,cse343,smartboard).
instructor(bayrakci,cse331,_).
instructor(gozupek,cse321,smartboard).


conflicts(CourseID1,CourseID2) :- occupancy(Class,Time,CourseID1) , occupancy(Clas,Tim,CourseID2) ,Class == Clas ,Time =:= Tim,CourseID1 \= CourseID2,write('There no conflict') .
% All class and time comparing for time conflict.

%Check which room can be assigned to a given class.
assign(RoomID,CourseID) :- room(RoomID,Y,_),course(CourseID,_,X,_,D), X =< Y ,D == RoomID,write('You can assign') .

%Check which room can be assigned to which classes.
assign(RoomID) :- course(X,_,_,_,RoomID),nl,format('~w suited' ,[X]),nl.

%Check whether a student can be enrolled to a given class.
enroll(StudentID,CourseID):- studentCourse(StudentID),course(CourseID,_,Capacity,_,_),X is 0,findCurrentCapacity(0,CourseID,X,Capacity),
							X>=Capacity,format('~w full capacity , you can not take ' ,[Capacity]),format('the ~w lecture' ,[CourseID]) 
							;format('~w can take ~w lecture' ,[StudentID,CourseID]). 

studentCourse(StudentID) :- student(StudentID,X,_),format('~w has ~w lecture',[StudentID,X]),nl.

findCurrentCapacity(S,CourseID,X,Capacity) :- S is S+1,student(S,CourseID,_),Y is X+1,CourseID is CourseID+1,findCurrentCapacity(S,CourseID,Y,Capacity) ;S is S+1, findCurrentCapacity(S,CourseID,X,Capacity) .
findCurrentCapacity(_,cse341,10,10) :-format('~w full capacity , you can not take ~w' ,[10,cse341]).
findCurrentCapacity(_,cse343,6,6) :- format('~w full capacity , you can not take ~w' ,[6,cse343]).
findCurrentCapacity(_,cse331,5,5) :- format('~w full capacity , you can not take ~w' ,[5,cse331]).
findCurrentCapacity(_,cse321,10,10) :-format('~w full capacity , you can not take ~w' ,[10,cse321]).

%Check which classes a student can be assigned.
enroll(StudentID) :-studentCourse(StudentID),format('~w student who',[StudentID]),format(', ~w full capacity , you can not take ~w' ,[10,cse341]),nl,
					 format('~w student who',[StudentID]),   format(', ~w full capacity , you can not take ~w' ,[6,cse343]),nl,
					 format('~w student who',[StudentID]),  format(', ~w full capacity , you can not take ~w' ,[5,cse331]),nl,
					 format('~w student who',[StudentID]),  format(', ~w not full capacity , you can take ~w' ,[6,cse321]),nl.


 
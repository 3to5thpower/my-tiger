; ModuleID = 'main'


 


@putNumForm =  unnamed_addr  constant [4 x i8] c"%d\0a\00"


declare external ccc  i32 @printf(i8*, ...)    


define external ccc  i32 @main()    {
  %1 = mul   i32 1, 3 
  %2 = sdiv  i32 2, 4 
  %3 = add   i32 %1, %2 
  %4 =  call ccc  i32 (i8*, ...) @printf(i8*  getelementptr inbounds ([4 x i8], [4 x i8]* @putNumForm, i32 0, i32 0), i32  %3)  
  ret i32 0 
}

nlist == [num]
nlist2 == [num]

pol1 = [1,2,3,4]|| 4x^3 + 3x^2 + 2x + 1
pol2 = [5,0,3]|| 3x²+5

||Function that evaluates a Polynomial, Horners Rule
polev :: (nlist, num) -> num
polev ([], num) = 0
polev ( front : rest, num ) = front + polev (rest, num)*num

||Evaluation of a derivate of a Polynomial
dpoli :: (nlist, num) -> num
dpoli ([], num) = 0
dpoli (front : rest, num)= polev(rest, num) + dpoli(rest,num)

||Composition of two polynomials
cpol::(nlist, nlist2) -> (nlist)


|| Sum of two Polynomials
sumpol :: (nlist, nlist2) -> (nlist)
sumpol ([],[])= []
sumpol ([],r2)= r2 
sumpol (r1,[])= r1
sumpol (f1:r1, f2:r2) = f1+f2: sumpol(r1,r2)

||Product of a real number to a Polynomial
appol :: (nlist, num) -> (nlist)
appol ([], num) = []
appol (f1:r1, num) = f1*num : appol(r1, num)

||Product of two Polynomials
ppol :: (nlist, nlist) -> (nlist)
ppol ([],[]) = []
ppol ([], nlist) = []
ppol (f1:r1, nlist) = sumpol (appol (nlist,f1), 0: ppol(r1,nlist))


||Symbolic print of a Polynomial
imppol :: nlist -> [char]
imppol any = creapol (any,0,"")
creapol :: (nlist, num,[char]) -> [char]
creapol ([], num,pol) = pol
creapol (front : rest, n,pol) = creapol (rest, n+1,"+"++show(front)++pol), if n=0  & front>0   
                              = creapol (rest, n+1,show(front)++pol), if n=0  & front<0  
                              = creapol (rest, n+1,""++pol), if n=0  & front=0

                              = creapol (rest, n+1,"+"++show(front)++"x"++pol), if n=1 &front>0    
                              = creapol (rest, n+1,""++pol), if n=1 & front=0
                              = creapol (rest, n+1,show(front)++"x"++show(n)++pol), if n=1 & front<0
                              = creapol (rest, n+1,"x"++pol), if n=1 & front=1  
                              = creapol (rest, n+1,"-"++"x"++pol), if n=1 & front=-1  
                   
                              = creapol (rest, n+1,""), if front=0
                              = creapol (rest, n+1,"+"++"x^"++show(n)++pol), if front=1 
                              = creapol (rest, n+1,"-"++"x^"++show(n)++pol), if front=-1
                              = creapol (rest, n+1,"+"++show(front)++"x^"++show(n)++pol), if (n>1) & front>0
                              = creapol (rest, n+1, show(front)++"x^"++show(n)++pol), if (n>1) & front<0
                              = creapol (rest, n+1, "" ++pol), if (n>1) & front=0

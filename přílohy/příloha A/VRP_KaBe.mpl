TITLE MTSP_VRP_KaBe;

OPTIONS
ExcelWorkBook="data.xlsx";
ExcelSheetname="vychodoceska";

INDEX
!i:=1..156;
!j:=1..156;
!p:=1..156;
i:=ExcelRange("i");
j:=ExcelRange("j");
p:=ExcelRange("i");
k=1..1;

DATA
c[i,j]:=ExcelRange("data");
n:=ExcelRange("n");
L=n;
K=n-1;

BINARY VARIABLES
!x[i,j,k] Export to ExcelRange("xxx");
x[i,j,k];

INTEGER VARIABLES
u[i];

MODEL
MIN Z = sum(i,j: c[i,j] * sum(k: x[i,j,k]) );

SUBJECT TO

navstivit[j>=2]: sum(i,k: x[i,j,k]) = 1;
ustalenost[k,p]:     sum(i: x[i,j:=p,k]) - sum(j: x[i:=p,j,k]) = 0;
salesman[k]:	 sum(j>=2: x[1,j,k]) = 1;

hornimez[i>=2]:  u[i] + sum(k: x[1,j:=i,k]*(L-2)) - sum(k: x[i,1,k]) <= L-1;
dolnimez[i>=2]:  u[i] + sum(k: x[1,j:=i,k]) + sum(k: x[i,1,k]*(2-K)) >= 2;
doplnek[i>=2]:   sum(k: x[1,j:=i,k]) + sum(k: x[i,1,k]) <= 1;

anticykl[i,j>=2] : u[i] - u[i:=j] + sum(k: L * x[i,j,k]) + sum(k: x[i:=j,j:=i,k]*(L-2)) <= L - 1;

END
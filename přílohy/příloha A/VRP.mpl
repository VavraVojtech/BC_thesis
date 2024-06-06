TITLE MTSP_VRP;

OPTIONS
ExcelWorkBook="data.xlsx";
ExcelSheetname="plzenska";

INDEX
!i:=1..156;
!j:=1..156;
!p:=1..156;
i:=ExcelRange("i");
j:=ExcelRange("j");
p:=ExcelRange("i");
k=1..3;

DATA
c[i,j]:=ExcelRange("data");
n:=ExcelRange("n");
L=10;

BINARY VARIABLES
!x[i,j,k] Export to ExcelRange("xxx");
x[i,j,k];

INTEGER VARIABLES
u[i];

MODEL
MIN Z = sum(i,j: c[i,j] * sum(k: x[i,j,k]) );

SUBJECT TO

navstivit[j>=2]:  sum(i,k: x[i,j,k]) = 1;
ustalenost[k,p]:  sum(i: x[i,j:=p,k]) - sum(j: x[i:=p,j,k]) = 0;
salesman[k]:	  sum(j>=2: x[1,j,k]) = 1;

anticykl[i,j>=2]: u[i] - u[i:=j] + L * sum(k: x[i,j,k]) <= L - 1;

END
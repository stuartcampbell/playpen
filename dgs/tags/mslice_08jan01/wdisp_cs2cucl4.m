function [w1,w2]=wdisp_cs2cucl4

J=0.64;
Jp=0.175*J;
s=1/2;
w1=sqrt((2*s*J+s*Jp^2/J-sqrt(2)*s*Jp^2/J)*(2*s*J+s*Jp^2/J+2*sqrt(2)*s*Jp));
w2=sqrt((2*s*J+s*Jp^2/J+sqrt(2)*s*Jp^2/J)*(2*s*J+s*Jp^2/J-2*sqrt(2)*s*Jp));
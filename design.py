# -*- coding: utf-8 -*-
"""
Created on Mon Jul 31 15:45:17 2017

@author: Martin Vasilev
"""

import os
import numpy as np

os.chdir('D:\R\Zstring')

if not os.path.exists('design'):
    os.makedirs('design')
    

for ID in range(1, 73):
    nsent= 180
    ncond= 2*3
    npos= 4 # num of target word positions
	#nlist= 3 # number of fully counter-balanced lists
	
    full_list= npos*ncond
	#nsub= nlist*full_list
    item= range(1,nsent+1)
    S1= np.arange(1, 73, 3)
    S2= np.arange(2, 73, 3)
    S3= np.arange(3, 73, 3)
	#S2= [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30, 32, 34, 36, 38, 40, 42, 44, 46, 48]
	
    P1= [1, 2, 9, 10, 17, 18, 25, 26, 33, 34, 41, 42, 49, 50, 57, 58, 65, 66]# 3, 13, 14, 15, 25, 26, 27, 37, 38, 39]
    P2= [3, 4, 11, 12, 19, 20, 27, 28, 35, 36, 43, 44, 51, 52, 59, 60, 67, 68] #6, 16, 17, 18, 28, 29, 30, 40, 41, 42]
    P3= [5, 6, 13, 14, 21, 22, 29, 30, 37, 38, 45, 46, 53, 54, 61, 62, 69, 70]#9, 19, 20, 21, 31, 32, 33, 43, 44, 45]
    P4= [7, 8, 15, 16, 23, 24, 31, 32, 39, 40, 47, 48, 55, 56, 63, 64, 71, 72]# 12, 22, 23, 24, 34, 35, 36, 46, 47, 48]
	
    B1= np.arange(1, 73, 2)
    B2= np.arange(2, 73, 2)
	
    if ID in S1:
        sound= ["SLC"]*int((nsent/ncond))+ ["STD"]*int((nsent/ncond)) + ["DEV"]*int((nsent/ncond))
        sound= sound*2
		
    if ID in S2:
        sound= ["STD"]*int((nsent/ncond))+ ["DEV"]*int((nsent/ncond)) + ["SLC"]*int((nsent/ncond))
        sound= sound*2
    
    if ID in S3:
        sound= ["DEV"]*int((nsent/ncond))+ ["SLC"]*int((nsent/ncond)) + ["STD"]*int((nsent/ncond))
        sound= sound*2
        
	####
	
    if ID in P1:
        pos= [2, 3, 4, 5]* int(nsent/npos)
	
    if ID in P2:
        pos= [3, 4, 5, 2]* int(nsent/npos)
		
    if ID in P3:
        pos= [4, 5, 2, 3]* int(nsent/npos)
		
    if ID in P4:
        pos= [5, 2, 3, 4]* int(nsent/npos)
	
	
    if ID in B1: # odd numbers
        block= ['reading']*int((nsent/2)) + ['zString']*int((nsent/2)) 
    else: # even numbers
        block= ['zString']*int((nsent/2)) + ['reading']*int((nsent/2)) 
	
    c= list(zip(item, sound, pos, block))
    c1= c[0:90]
    c2= c[90:180]
    matching_c1 = [s for s in c1 if s[1]=="SLC"]
    not_matching_c1= [s for s in c1 if s[1]!="SLC"]
	
    STD = [i for i, s in enumerate(not_matching_c1) if 'STD' in s[1]]
    first_c1= [not_matching_c1[STD[0]]]+ [not_matching_c1[STD[1]]]+ [not_matching_c1[STD[2]]]
    del not_matching_c1[STD[0]]
    del not_matching_c1[STD[1]-1]
    del not_matching_c1[STD[2]-2]
	
	# randomise sounds' block:
    from random import shuffle
    shuffle(not_matching_c1)
    shuffle(matching_c1)
    
    
    matching_c2 = [s for s in c2 if s[1]=="SLC"]
    not_matching_c2= [s for s in c2 if s[1]!="SLC"]
    STD = [i for i, s in enumerate(not_matching_c2) if 'STD' in s[1]]
    first_c2= [not_matching_c2[STD[0]]]+ [not_matching_c2[STD[1]]]+ [not_matching_c2[STD[2]]]
    del not_matching_c2[STD[0]]
    del not_matching_c2[STD[1]-1]
    del not_matching_c2[STD[2]-2]
	
	# randomise sounds' block:
    shuffle(not_matching_c2)
    shuffle(matching_c2)

    if ID in B1:
        PRAC1= [(181, 'PRAC', 1, 'reading'), (182, 'PRAC', 1, 'reading'), (183, 'PRAC', 1, 'reading'), \
	        (184, 'PRAC', 1, 'reading'), (185, 'PRAC', 1, 'reading'), (186, 'PRAC', 1, 'reading')]
        PRAC2= [(187, 'PRAC', 1, 'zString'), (188, 'PRAC', 1, 'zString'), (189, 'PRAC', 1, 'zString'), \
	        (190, 'PRAC', 1, 'zString'), (191, 'PRAC', 1, 'zString'), (192, 'PRAC', 1, 'zString')]
    else: 
        PRAC1= [(181, 'PRAC', 1, 'zString'), (182, 'PRAC', 1, 'zString'), (183, 'PRAC', 1, 'zString'), \
	        (184, 'PRAC', 1, 'zString'), (185, 'PRAC', 1, 'zString'), (186, 'PRAC', 1, 'zString')]
        PRAC2= [(187, 'PRAC', 1, 'reading'), (188, 'PRAC', 1, 'reading'), (189, 'PRAC', 1, 'reading'), \
	        (190, 'PRAC', 1, 'reading'), (191, 'PRAC', 1, 'reading'), (192, 'PRAC', 1, 'reading')]

        
    shuffle(PRAC1)
    shuffle(PRAC2)
    
    WB1= np.arange(1, 73, 4)
    WB2= np.arange(2, 73, 4)
    WB3= np.arange(3, 73, 4)
    WB4= np.arange(4, 73, 4)
	
    if ID%2==1: # odd numbers
        if ID in WB1:
            design= PRAC1+ matching_c1+ first_c1+not_matching_c1 + PRAC2+ matching_c2+ first_c2+not_matching_c2
        else:
            design= PRAC1+ first_c1+not_matching_c1+ matching_c1 + PRAC2+ first_c2+not_matching_c2+ matching_c2
    else: # even numbers
        if ID in WB2:
            design= PRAC1+ matching_c1+ first_c1+not_matching_c1 + PRAC2+ matching_c2+ first_c2+not_matching_c2
        else:
            design= PRAC1+ first_c1+not_matching_c1+ matching_c1 + PRAC2+ first_c2+not_matching_c2+ matching_c2
            
    print(design)
	
    thefile = open('Design/P'+ str(ID)+ '.txt', 'w')
    thefile.write("item sound pos task\n") # columns
	
    for item in design:
        thefile.write("%s %s %s %s\n" % item)
    thefile.close()
	
	
	# questions:
	# 13 between 1-40, 13 between 41-80, 13 between 81-120
		

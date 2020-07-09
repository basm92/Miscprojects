#!/usr/bin/env python
# coding: utf-8

# In[2]:


import os as os


# # Beginning of my code
# 
# 
# ## It is very beautiful
# 
# In this code, I am writing very interesting things. For example, I am finding out how to drink beer. 
# 
# Next, I also hope to show how you can be a good Python programmer. 
# 
# Is it allowed to write $\LaTeX$ in here?
# 
# $4x = cos(\beta)$
# 
# 

# In[29]:


os.getcwd()
print(os.getcwd())

file = open('./Erasmus/liviopietersiward.md')

test = file.read()

print(test)


# paul_ek = pd.read_csv("/home/bas/Documents/paul_ek.csv")

# In[17]:


paul_ek.describe(include='all')


# In[15]:


largest = None
smallest = None

while True:
    num = input("Enter a number: ")
    
    if num == "done" : 
        break
    
    try :
    	num = float(num)
    
    except :
        print('Invalid input')
        continue
    
    if smallest is None :
        smallest = num
    elif smallest > num :
        smallest = num
    
    if largest is None :
        largest = num
    elif largest < num :
        largest = num
 

print("Maximum is", largest)
print("Minimum is", smallest)


# In[ ]:





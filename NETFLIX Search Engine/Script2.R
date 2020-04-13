##########################################################################################################################################################################################
#1. This script we are running in the Shiny R (servre.R) file.
#2. This script will run under the reactive widget. i.e. whenever the user changes the input,
#   the input will be passed in to a function. The function runs this main script ,
#   which takes the input -moviename( which user has selected on front-end) and process the output.
#########################################################################################################################################################################################


######################## This code is by Mechin David#############################################



moviefunc <- function(moviename){
  
  recommendation_DT <- merge(movies,credits,by="title")  
  
  for(ee in 1:nrow(recommendation_DT)){
    
    if(strcmp(moviename,(recommendation_DT[ee,1])))
    {
      my_movie_row=recommendation_DT[ee,5]
      break
    }
  }
  
user_movie_id <- as.integer(my_movie_row) # converting it to integer
  
search_engine_df=data.frame() # making a null dataframe to store our results

############################################################################################################################################################################
################### to identify the Director Name###########################################################################################################################
############################################################################################################################################################################  

######################## This code is by Mechin David#############################################

i=0 #initialising
  
y=which(colnames(recommendation_DT)=="movie_id" )
z=which(colnames(recommendation_DT)=="crew" )
  
for(i in 1:nrow(recommendation_DT)){
    y=which(colnames(recommendation_DT)=="movie_id" )
    z=which(colnames(recommendation_DT)=="crew" )
    if(user_movie_id==as.integer(recommendation_DT[i,y])){
      info=recommendation_DT[i,z]
      info=as.character(info)
      info=gsub("[^[:alnum:][:space:]',:]", "", info)
      info2=as.list(strsplit(info, ",")[[1]])
      for(u in 1:length(info2))
      {
        if (info2[u]==" job: Director")
        {
          row=u;
          break
        }
      }
      directorname=info2[row+1]
      t=tolower(trimws(gsub("^.*?:","",directorname)))
      break}
  }
x=as.vector(t)

############################################################################################################################################################################
##################################### to identify the actor names###########################################################################################################
############################################################################################################################################################################
# we can take i value directly from the above because, once we run the directors loop ,
# we will get the ID which matches with the movie title

######################## This code is by Mechin David#############################################

  
for(i in 1:nrow(recommendation_DT)){
    actorslist=NULL
    y=which(colnames(recommendation_DT)=="movie_id" )
    z=which(colnames(recommendation_DT)=="cast" )
    if(user_movie_id==recommendation_DT[i,y]){
      info=as.character(recommendation_DT[i,z])
      info=gsub("[^[:alnum:][:space:]',:]", "", info)
      info2=as.list(strsplit(info, ",")[[1]])
      count=0
      actors=vector()
      for(u in 1:length(info2))
      {
        if(grepl("name:",info2[u]))
        {
          count=count+1
          actorname=info2[u]
          t=trimws(gsub("^.*?:","",actorname))
          actors[count]=t
        }
        if(count==16)
        {
          break
        }
      }
      actorslist=as.data.frame(actors)
      y=t(as.vector(actorslist))
      break}
  }
  s=NULL
  s=cbind(x,y)
  s=data.frame(s,stringsAsFactors = FALSE)
  
################################################################################################################################################################################################
################################## To identify the top genre###########################################################################################################
################################################################################################################################################################# 

######################## This code is by Shyangyue Fu #############################################
  
  
  
  #to execute below import the frequent genre csv which we have found

# if you are already running the Script1 before this , below step is not necessary
#freq_genre=read.csv("frequent_genre.csv" ,header = TRUE,sep= ",")
# here we use the same "i" which we found when finding the directors
  
  p=which(colnames(recommendation_DT)=="genres" )
  
  genrelist=recommendation_DT[i,p]
  genrelist=as.character(genrelist)
  genrelist=gsub("[^[:alnum:][:space:]',:]", "", genrelist)
  genrelist=as.list(strsplit(genrelist, ",")[[1]])
  
  final_genre=vector()
  final_genre=NULL
  genre_count=1
  q=0
  for (q in 2:length(genrelist)) {
    gl=gsub("^.*?:","",genrelist[q])
    gl=trimws(gl)
    gl=tolower(gl)
    q=q+2
    if(gl %in% frequent_genre$frequent_genre){
      
      final_genre[genre_count]=gl
      gl=NULL
      genre_count=genre_count+1
    }
  }  
  final_genre=as.data.frame(final_genre)  
  
  r=t(as.vector(final_genre))
  r=data.frame(r,stringsAsFactors = FALSE)
  
  
  s=cbind(s,r)
  s=data.frame(s,stringsAsFactors = FALSE)
  
  ###################################################################################################################################################################
  ################################# picking top 20 keywords and adding to dataframe##################################################################################
  ###################################################################################################################################################################

  ######################## This code is by Shyangyue Fu ############################################# 
    
  # if you are already running the Script1 before this , below step is not necessary
  
  #KeyWords_Engine=read.csv("KeyWords_Engine.csv" ,header = TRUE,sep= ",")
  KeyWords_Engine$X=NULL
  
  # here we use the same "i" which we found when finding the directors
  
  g=which(colnames(recommendation_DT)=="keywords" )
  keywordslist=recommendation_DT[i,g]
  keywordslist=gsub("[^[:alnum:][:space:]',:]", "", keywordslist)
  keywordslist=as.list(strsplit(keywordslist, ",")[[1]])
  
  
  final_keylist=vector()
  h=0
  kw_count=1
  for (h in 2:length(keywordslist)) {
    gl=gsub("^.*?:","",keywordslist[h])
    gl=trimws(gl)
    gl=tolower(gl)
    h=h+2
    if(gl %in% KeyWords_Engine$Words){
      final_keylist[kw_count]=gl
      gl=NULL
      kw_count=kw_count+1
      if(kw_count>=21){
        break  }  
    }
  }  
  final_keylist=t(final_keylist)
  final_keylist=data.frame(final_keylist,stringsAsFactors = FALSE)
  
  
  s=cbind(s,final_keylist)
  s=data.frame(s,stringsAsFactors = FALSE)
  
###################################################################################################################################################################
##################################### building  a matrix with 1's and 0's##########################################################################################
###################################################################################################################################################################

  ######################## This code is by Shyangyue Fu #############################################  
  
rowsofdt=nrow(recommendation_DT)
  dt_rows=length(s)
  recommendation_DT$new =paste(recommendation_DT$crew,recommendation_DT$cast,recommendation_DT$genres,recommendation_DT$keywords)
  recommendation_DT$new=gsub("^.*?:","",tolower(recommendation_DT$new))
  recommendation_DT$new=gsub("[^[:alnum:][:space:]',:]", "",recommendation_DT$new)
  m=nrow(s)
  
  for(b in 1:rowsofdt)#looping in entire movies+credits dataset
  {    
    q=which(colnames(recommendation_DT)=="new")
    u=which(colnames(recommendation_DT)=="id")
    qq=which(colnames(recommendation_DT)=="title")
    crew_text_from=recommendation_DT[b,q]
    m=nrow(s)
    m=m+1
    for (w in 1:dt_rows)
    {
      
      ifelse(grepl(tolower(s[1,w]),crew_text_from,fixed=TRUE),s[m,w]<-"1",s[m,w]<-"0")
    }
  }
  
  #Making First Row as Column Names, removing First Row and Making Row Names NULL
  names(s) <-  unlist(s[1,])
  s <- s[-1,]
  row.names(s) <- NULL

# Binding the id,title,release date and popularity to pull-out records and arrange based on releasedate and popularity  
  s= cbind(s,recommendation_DT$id)
  s= cbind(s,recommendation_DT$title)
  s= cbind(s,recommendation_DT$release_date)
  s= cbind(s,recommendation_DT$popularity)
  
# Renaming the columns for readbility
  
  colnames(s)[length(s)]<- "popularity"
  colnames(s)[length(s)-1]<- "releasedate"
  colnames(s)[length(s)-2]<- "title"
  colnames(s)[length(s)-3]<- "movie_id"
  
##############################################################################################################################################################
############################### To calculate the distance ####################################################################################################
##############################################################################################################################################################

###################### This code is by Kishore Kumar Biradavolu #############################################  
  
    
# we are saving it in another dataframe and perfoming changes so as not to lose the main matrix.
  
  engine=s

# removing all the o=popularity,id,names,releasedates as they are not necessary in distance finding ans also as they are not Numeric. 
  engine[,length(engine)]=NULL#removing popularity
  engine[,length(engine)]=NULL#removing release year
  engine[,length(engine)]=NULL # for removing the title
  engine[,length(engine)]=NULL# removing the movie id
  
# Building user selected dataframe  
  usermovie=data.frame()

  for (w in 1:ncol(engine)) {
    engine[,w] <- as.numeric(engine[,w])
    usermovie[1,w]=1
    usermovie[1,w] <- as.numeric(usermovie[1,w])
  }
# Naming usenmovie dataframe column names exactly to the columns which we found out and assigned to the matrix
names(usermovie)=colnames(engine)

# Convertig the usermovie matrix and matrix we found into class=matrix to find-out distances.
#For finding distances, all the DataFrames should be in matrix forms and all columns of the DataFrames should be numerics 

usermovie=as.matrix(usermovie)
engine <- as.matrix(engine)
  
  
  
  qwerty= as.matrix(dist(engine,usermovie, method ="manhattan"))
  qwerty=qwerty[1,1:ncol(as.data.frame(qwerty))]
  
row.names(qwerty)=NULL
  
#Binding back all the oter columns to find the recommendations on popularity and releasedYear
  qwerty_id=cbind(qwerty,s$movie_id)
  qwerty_id=cbind(qwerty_id,s$title)
  qwerty_id=cbind(qwerty_id,s$releasedate)
  qwerty_id=cbind(qwerty_id,s$popularity)
  
  qwerty_id$qwerty=as.numeric(qwerty_id$qwerty)
  
  rownames(qwerty_id)=NULL
  rownames(s)<-c()

#Arranging in descending order
  
  qwerty_id=qwerty_id[order(-qwerty_id[,1]), ]
  
# Below dataframe to store the recommendations
  
  rec_movies=data.frame(stringsAsFactors = FALSE)
  for(a in 1: 6)
  {
    rec_movies=rbind(rec_movies,qwerty_id[a,])
  }
  
  rec_movies$`s$releasedate`=as.character(rec_movies$`s$releasedate`)
  rownames(rec_movies)=NULL
  
  for(a in 1: 6)
  {
    rec_movies[a,4]=substring(rec_movies[a,4],1,4)
  }
  
  rec_movies$`s$releasedate`=as.integer(rec_movies$`s$releasedate`)
  
  
  g1=data.frame(stringsAsFactors = FALSE)
  g2=data.frame(stringsAsFactors = FALSE)
  g3=data.frame(stringsAsFactors = FALSE)
  
  g1=rbind(g1,rec_movies[1,])
  
  for(a in 2: 6)
  {
    if(rec_movies[a,4]>=1951){g2=rbind(g2,rec_movies[a,])}
    else if(rec_movies[a,4]>=1916 && rec_movies[a,4]<=1950){g3=rbind(g3,rec_movies[a,])}
  }
  
# group by popularity
  
  if(length(g2)>0){
    g2=g2[order(-g2[,5]), ]
    final_recommendations=rbind(final_recommendations,g1)
    final_recommendations=rbind(final_recommendations,g2)
  }
  if(length(g3)>0){
    g3=g3[order(-g3[,5]), ]
    final_recommendations=rbind(final_recommendations,g2)
  }
  
  final_recommendations=data.frame(stringsAsFactors = FALSE)

  rownames(final_recommendations)=NULL

  data2=data.frame(final_recommendations$`s$title`)
  
  # Returning this variable as the entire code is a function in ShinyR and it should return someoutput.
  return(data2)
  
}
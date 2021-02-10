#### `notebookR`
####
#### Author:  Peter Hoff
#### License: GPL-3 
#### 
#### Shiny app to render and serve a searchable webpage of Rmarkdown notes.
####
#### How to:
#### 1. Put this app in a directory, say `/Users/me/Notebook` 
#### 2. Make a subdirectory `www`.
#### 3. Make a subdirectory `content`.
#### 4. Put markdown or Rmarkdown files in `content`, but use 
####    `.md` file extension and not `.Rmd`. 
#### 5. Run the app: `runApp(appDir="/Users/me/Notebook",port=6984,host='127.0.0.1',quiet=TRUE)` 
#### 6. Go to `http://127.0.0.1:6984/`  in a web browser. 
#### 
#### Its best to run the app in the background, so you can access the 
#### the app without having an "active" R sesssion. 

#### ---- 
render<-function(){  

  ## md files
  mdFiles<-system("stat -l -t '%FT%T' content/*.md",intern=TRUE) 
  mark<-regexpr("content",mdFiles) 
  mark2<-sapply(gregexpr("\\.",mdFiles),max) 
  mdNames<-substring(mdFiles,mark+8,mark2-1) 
  mdTimes<-gsub("T"," ",substring(mdFiles,mark-20,mark-2))
  mdTimes<-strptime(mdTimes,"%Y-%m-%d %H:%M:%S")

  ## html files
  hFiles<-system("stat -l -t '%FT%T' www/*.html",intern=TRUE)
  mark<-regexpr("www",hFiles)
  hNames<-substring(hFiles,mark+4,nchar(hFiles)-5)                  
  hTimes<-gsub("T"," ",substring(hFiles,mark-20,mark-2))
  hTimes<-strptime(hTimes,"%Y-%m-%d %H:%M:%S")
 
  ## remove defunct html files
  rmhtml<-match(setdiff(hNames,mdNames),hNames) 
  if(length(rmhtml)>0)
  {
    for(i in rmhtml){ system(paste0("rm -rf www/",hNames[i],"*")) }
    hTimes<-hTimes[ -rmhtml ] 
    hNames<-hNames[ -rmhtml ] 
  }

  ## see which files need to be rendered

  #### match md to html
  frend<-match(mdNames,hNames)  

  ### check to see if md has been updated since last render
  frend[!is.na(frend)]<-unlist( 
        mapply( function(x,y){ difftime(x,y)>0 }, 
        mdTimes[!is.na(frend)],
        hTimes[frend[!is.na(frend)]] )   )
 
  ### if not matched then need to render 
  frend[is.na(frend)]<-TRUE 


  ## read yaml, render things that need to be rendered 
  YAML<-list() 
  for(i in seq_along(mdFiles)) 
  {
    mdfname<-paste0("content/",mdNames[i],".md") 
    YAML[[i]]<-rmarkdown::yaml_front_matter(mdfname) 
    if(frend[i]){  
      tf<-paste0("content/",mdNames[i],".Rmd") 
      system(paste0("cp ",mdfname," ",tf))
      rmarkdown::render(tf,output_file=paste0(mdNames[i],".html"),
                        output_dir="www",envir=new.env())
      system(paste0("rm ",tf)) 
    }
  }

}

#### ---- 
ui<-fluidPage(

# uncomment below for a close button
#  tags$button(
#    id = 'close',
#    type = "button",
#    class = "btn action-button",
#    onclick = "setTimeout(function(){window.close();},500);",  # close browser
#    "close"
#    ),
  textInput("value", "", ""),
  uiOutput("links")
)


#### ----
server<-function(input, output, session){

# uncomment below for a close button
# observe({ if (input$close>0) stopApp() }) 


  output$links <- renderText( { 
    render()

    ## -- files
    hd<-"~/Dropbox/Journal/www/"
    hfiles<-system(paste("ls",hd),intern=TRUE) 
    hfiles<-substr(hfiles,1,nchar(hfiles)-5)    
 
    md<-"~/Dropbox/Journal/content/"
    files<-mfiles<-system(paste("ls",md),intern=TRUE) 
    files<-mfiles<-files[ tools::file_ext(files)=="md" ]
    mfiles<-substr(mfiles,1,regexpr("\\.",mfiles)-1 ) 
    files<-files[ is.element(mfiles,hfiles) ] 
    

    ## -- find matches
    matches<-list()
    for(i in seq_along(files)){
      x<-grep(input$value,
              readLines(paste0(md,files[i])),ignore.case=TRUE,value=TRUE)
      if(length(x)>0){ 
        mdata<-list()

        title<-grep("title:",
               readLines(paste0(md,files[i])),value=TRUE)[1]
        title<-trimws(substring(title,7))

        date<-grep("date:",
               readLines(paste0(md,files[i])),value=TRUE)[1] 
        date<-trimws(substring(date,6))

        mdata<-list() ; 
        mdata$date<-date ; mdata$title<-title ; mdata$file<-files[i] 

        matches[[length(matches)+1]]<-mdata  
      } 
    }

    ## -- construct output    
    x<-c("<table>  <tr>  <th>  </th> <th>  </th> </tr>")
    for(i in rev(seq_along(matches))){ 
      link<-matches[[i]]$file  
      link<-paste0(substr(link,1,nchar(link)-2),"html") 
      
      x<-paste(x,"<tr> <td>",matches[[i]]$date," &nbsp; </td>",
                      "<td> <a href=",link,">",
                             matches[[i]]$title,"</a></td>" ) 
                        
    }

    x
  } )
}

#### ----
shinyApp(ui, server) 



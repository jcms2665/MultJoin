#' Esta función crea sub-bases y cambia de formato algunas variables
#' @title Manipulando bases de datos
#' @name ENOEString
#' @param base1 Base de datos 1 
#' @param base2 Base de datos 2
#' @param base3 Base de datos 3
#' @param base4 Base de datos 4
#' @param base5 Base de datos 5
#' @param u Variable 
#' @export
ENOEString<-function(base1,base2,base3,base4,base5,u){
  k<-c("CD_A","ENT","CON","V_SEL","N_HOG","H_MUD","N_REN")
  r<-2
  for(base in paste("base",2:5,sep="")){
    h<-get(base)
    h$SEX<-as.numeric(as.character(h$SEX))
    h$R_DEF<-as.numeric(as.character(h$R_DEF))
    h$C_RES<-as.numeric(as.character(h$C_RES))
    #h$CLASE2<-as.numeric(as.character(h$CLASE2))
    h$N_ENT<-as.numeric(as.character(h$N_ENT))
    
    h<-subset(h,h$SEX==2 & h$N_ENT==r & (h$R_DEF==0 & (h$C_RES==1 | h$C_RES==3)),select = c(k,u))
    names(h)<-c(k,paste(u,r,sep=""))

    base1<-merge(base1,h,by=k, all.x = TRUE)
    assign("BaseGral",base1,envir = .GlobalEnv)
    #assign(paste("temp",r,sep=""), h,envir = .GlobalEnv)
    r=r+1
  }
}










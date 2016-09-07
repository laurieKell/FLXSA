setGeneric('diags',   function(object,method,...) standardGeneric('diags'))
setMethod('diags',  signature(object='FLXSA',method="missing"), 
  function(object,method) {
  
  res=mdply(data.frame(i=seq(length(object@index))), function(i){
    model.frame(FLQuants(obs     =object@index[[i]]%/%object@q.hat[[i]],
                         hat     =object@stock.n[dimnames(index(object)[[i]])$age,
                                                 dimnames(index(object)[[i]])$year],
                         residual=index.res(object)[[i]]),drop=T)})
  
  res=ddply(res,.(i,age),diags:::diagsFn)
  names(res)[1]="name"
  #res=transform(res,name=names(u)[i])
  res})

setMethod('plot', signature(x='FLXSA', y='missing'),function(x){
  
  ggplot(subset(x@diagnostics,age>0))+
    geom_point(aes(as.numeric(yrcls),nhat,size=0),shape=1,alpha=0)+
    geom_point(aes(as.numeric(yrcls),nhat,fill=factor(age),size=w),shape=21,
               data=subset(x@diagnostics,age>0&w>0))+
    geom_vline(aes(xintercept=range(ple4.xsa)["maxyear"]-ple4.xsa@control@shk.yrs-0-5))+
    facet_grid(source~age)+
    theme_bw()+theme(legend.position="none")})
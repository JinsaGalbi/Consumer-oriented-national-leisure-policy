##
setwd('c:/Users/USER/Desktop/data')
library(dplyr)
library(magrittr)
library(tidygraph)
library(ggraph)
library(tidyverse)
library(igraph)
group1 <- data.table::fread('network111.csv')
group2 <- data.table::fread('network222.csv')
group3 <- data.table::fread('network333.csv')
group4 <- data.table::fread('network444.csv')

# 청년층
from <- group3 %>%
  distinct(from) %>%
  rename(label = from)
to <- group3 %>%
  distinct(to) %>%
  rename(label = to)
nodes <- full_join(from, to, by = "label")
nodes[,1]<- nodes[order(nodes$label),]
nodes <- nodes %>% rowid_to_column("id")
nodes%<>%tibble()

# edge
wedge3 <- group3 %>%  
  group_by(from, to) %>%
  summarise(weight = n()) %>% 
  ungroup()
wedge3 <- wedge3[order(wedge3$weight),]
quantile(wedge3$weight)
wedge3%<>%filter(weight>=18)
#
a <- wedge3%>%select(from)%>%unique()
b <- wedge3%>%select(to)%>%unique() ; colnames(b) <- 'from'
node<- rbind(a,b)%>%unique()%>%arrange(from) ; node%<>%mutate(id=1:16,label=from)%>%select(-from)
routes_igraph <- graph_from_data_frame(d = wedge3, vertices = node$label, directed = F)
## centrality
cen1<- wedge3 %>% as_tbl_graph() %>%
  mutate(deg= degree(routes_igraph),bet=betweenness(routes_igraph),eig=centrality_eigen()) %>% as_tibble %>% arrange(desc(eig))
apply(cen1[,-1],2,quantile) 
degree(routes_igraph)
betweenness(routes_igraph)
eigen_centrality(routes_igraph)$vector

range <- function(x){(x-min(x))/(max(x)-min(x))}
V(routes_igraph)$size<-(eigen_centrality(routes_igraph)$vector+range(degree(routes_igraph))+range(betweenness(routes_igraph)))*20


#시각화
#
V(routes_igraph)$size<-exp(eigen_centrality(routes_igraph)$vector)*20 # 노드의 중심성에 가중치(노드의 크기)
V(routes_igraph)$size<-degree(routes_igraph)*5+10 # 노드의 중심성에 가중치(노드의 크기)
E(routes_igraph)$width <- E(routes_igraph)$weight/5 # 연결선에 가중치
colrs <- c('#81F7D8','#C8FE2E','#F78181','#C8FE2E','#C8FE2E','#F781F3','#FACC2E','#81F7D8','#81F7D8','#C8FE2E','#F78181','#F78181','#F78181','#81F7D8','#FACC2E','#C8FE2E') 
V(routes_igraph)$color <- colrs[V(routes_igraph)]
plot(routes_igraph, edge.color='gray80',vertex.frame.color=NA,vertex.label.font=2,vertex.label.cex=1.4,edge.lty=1)

# 노년층
from <- group2 %>%
  distinct(from) %>%
  rename(label = from)
to <- group2 %>%
  distinct(to) %>%
  rename(label = to)
nodes <- full_join(from, to, by = "label")
nodes[,1]<- nodes[order(nodes$label),]
nodes <- nodes %>% rowid_to_column("id")
nodes%<>%tibble()

# edge
wedge2 <- group2 %>%  
  group_by(from, to) %>%
  summarise(weight = n()) %>% 
  ungroup()
wedge2 <- wedge2[order(wedge2$weight),]
quantile(wedge2$weight)
wedge2%<>%filter(weight>=12)
#
a <- wedge2%>%select(from)%>%unique()
b <- wedge2%>%select(to)%>%unique() ; colnames(b) <- 'from'
node<- rbind(a,b)%>%unique()%>%arrange(from) ; node%<>%mutate(id=1:19,label=from)%>%select(-from)
routes_igraph <- graph_from_data_frame(d = wedge2, vertices = node$label, directed = F)
## centrality
cen1<- wedge2 %>% as_tbl_graph() %>%
  mutate(deg= centrality_degree(),bet=centrality_betweenness(),eig=centrality_eigen()) %>% as_tibble %>% arrange(desc(eig))
apply(cen1[,-1],2,quantile)

#시각화
#
V(routes_igraph)$size<-exp(eigen_centrality(routes_igraph)$vector)*20 # 노드의 중심성에 가중치(노드의 크기)
E(routes_igraph)$width <- E(routes_igraph)$weight/3+4 # 연결선에 가중치
colrs <- c('#FFFF00','#C8FE2E','#58D3F7','#81F7D8','#58D3F7','#58D3F7','#F78181','#58D3F7','#81F7D8','#58D3F7','#81F7D8','#58D3F7','#C8FE2E','#81F7D8','#C8FE2E','#58D3F7','#C8FE2E','#58D3F7','#81F7D8') 
V(routes_igraph)$color <- colrs[V(routes_igraph)]
plot(routes_igraph, edge.color='gray80',vertex.frame.color=NA,vertex.label.font=2,vertex.label.cex=1.4,edge.lty=1)

# YOLO 중년층
from <- group1 %>%
  distinct(from) %>%
  rename(label = from)
to <- group1 %>%
  distinct(to) %>%
  rename(label = to)
nodes <- full_join(from, to, by = "label")
nodes[,1]<- nodes[order(nodes$label),]
nodes <- nodes %>% rowid_to_column("id")
nodes%<>%tibble()

# edge
wedge1 <- group1 %>%  
  group_by(from, to) %>%
  summarise(weight = n()) %>% 
  ungroup()
wedge1 <- wedge1[order(wedge1$weight),]
quantile(wedge1$weight)
wedge1%<>%filter(weight>=10)
#
a <- wedge1%>%select(from)%>%unique()
b <- wedge1%>%select(to)%>%unique() ; colnames(b) <- 'from'
node<- rbind(a,b)%>%unique()%>%arrange(from) ; node%<>%mutate(id=1:18,label=from)%>%select(-from)
routes_igraph <- graph_from_data_frame(d = wedge1, vertices = node$label, directed = F)
## centrality
cen1<- wedge1 %>% as_tbl_graph() %>%
  mutate(deg= centrality_degree(),bet=centrality_betweenness(),eig=centrality_eigen()) %>% as_tibble %>% arrange(desc(eig))
apply(cen1[,-1],2,quantile)

#시각화
#
V(routes_igraph)$size<-exp(eigen_centrality(routes_igraph)$vector)*18 # 노드의 중심성에 가중치(노드의 크기)
E(routes_igraph)$width <- E(routes_igraph)$weight/2+6 # 연결선에 가중치
colrs <- c('#C8FE2E','#58D3F7','#81F7D8','#F781F3','#58D3F7','#819FF7','#81F7D8','#58D3F7','#58D3F7','#819FF7','#81F7D8','#81F7D8','#58D3F7','#C8FE2E','#F78181','#C8FE2E','#58D3F7','#C8FE2E')
V(routes_igraph)$color <- colrs[V(routes_igraph)]
plot(routes_igraph, edge.color='gray80',vertex.frame.color=NA,vertex.label.font=2,vertex.label.cex=1.4,edge.lty=1)


# 고소득 중년층
from <- group4 %>%
  distinct(from) %>%
  rename(label = from)
to <- group4 %>%
  distinct(to) %>%
  rename(label = to)
nodes <- full_join(from, to, by = "label")
nodes[,1]<- nodes[order(nodes$label),]
nodes <- nodes %>% rowid_to_column("id")
nodes%<>%tibble()

# edge
wedge4 <- group4 %>%  
  group_by(from, to) %>%
  summarise(weight = n()) %>% 
  ungroup()
wedge4 <- wedge4[order(wedge4$weight),]
quantile(wedge4$weight)
wedge4%<>%filter(weight>=17)
#
a <- wedge4%>%select(from)%>%unique()
b <- wedge4%>%select(to)%>%unique() ; colnames(b) <- 'from'
node<- rbind(a,b)%>%unique()%>%arrange(from) ; node%<>%mutate(id=1:19,label=from)%>%select(-from)
routes_igraph <- graph_from_data_frame(d = wedge4, vertices = node$label, directed = F)
## centrality
cen1<- wedge4 %>% as_tbl_graph() %>%
  mutate(deg= centrality_degree(),bet=centrality_betweenness(),eig=centrality_eigen()) %>% as_tibble %>% arrange(desc(eig))
apply(cen1[,-1],2,quantile)

#시각화
#
V(routes_igraph)$size<-exp(eigen_centrality(routes_igraph)$vector)*18 # 노드의 중심성에 가중치(노드의 크기)
E(routes_igraph)$width <- E(routes_igraph)$weight/4+4 # 연결선에 가중치
colrs <- c('#C8FE2E','#58D3F7','#81F7D8','#C8FE2E','#58D3F7','#F78181','#58D3F7','#58D3F7','#FACC2E','#81F7D8','#58D3F7','#58D3F7','#C8FE2E','#F78181','#F78181','#58D3F7','#C8FE2E','#58D3F7','#81F7D8')
V(routes_igraph)$color <- colrs[V(routes_igraph)]
plot(routes_igraph, edge.color='gray80',vertex.frame.color=NA,vertex.label.font=2,vertex.label.cex=1.4,edge.lty=1)



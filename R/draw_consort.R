#' Draw Consort
#'
#' Draws a flowchart or consort diagram
#' @param data data.frame with the following columns
#' @param arrows data.frame with the following columns
#' @param phantom data.frame with the following columns
#' @keywords flowchart, consort
#' @export
#' @examples
#' #


# tgsify
draw_consort <- function(data, arrows, phantom = NULL){
  # INPUTS
  # data - data.frame with the following columns
  #        node      - node number
  #        row       - number - the row where the node is to be placed
  #        col       - number - the column where the node is to be placed
  #        label     - Text for node
  #        N         - Number in node
  #        sum_node  - (DISCONTINUED) node of parent(s) - use c(node1, node2, ...) for multi parent nodes
  #        plot      - logical - plot the node?
  #        type      - (optional) number (boxes of the same type are the same size)
  #        color     - (optional) color of box, default is white, NA is white
  # arrows - data.frame with the following columns
  #          from - number of node (arrow start)
  #          to   - number of node (arrow end)
  # phantom - data.frame with the following columns
  #           node - number - identifier of the node
  #           row  - number - row where phantom node is to appear
  #           col  - number - col where the phantom node is to appear
  #
  #
  # ADD title with title(main=title, outer=TRUE)
  # ADD text in top right corner with   mtext(format(Sys.time(), "%Y-%m-%d %H:%M"),3,outer=TRUE,adj=1)

  # ADD PHANTOM NODES TO DATA
  if(!is.null(phantom) & is.data.frame(phantom)){
    # Remove phantom nodes that are duplicated in the data (row, col)
    for(i in 1:nrow(phantom)){
      idx <- data[,"row"] %in% phantom[i, "row"] &
        data[, "col"] %in% phantom[i,"col"]
      if(sum(idx) > 0){
        warning("Phantom node is also in consort data.  Duplicate node ignored.")
        phantom <- phantom[-i, ]
      }
    }
    
    # Remove phantom nodes that are duplicated in the data (node)
    idx <- phantom[,"node"] %in% data[,"node"]
    if(sum(idx)){
      warning("Phantom node is also in consort data.  Duplicate phantom node ignored.")
      phantom <- phantom[-which(idx), ]
    }
    
    if(nrow(phantom)>0){
      data <-
        phantom %>%
        mutate(plot = FALSE) %>%
        mutate(type = -1) %>%
        as.data.table %>% 
        rbind(
          as.data.table(data)
          , fill=TRUE
          , use.names = TRUE) %>%
        as.data.frame() %>%
        arrange(node)
    }
  }
  
  
  # DEFAULTS
  if(is.null(data$color)) data$color <- "#FFFFFF"
  data$color[is.na(data$color)] <- "#FFFFFF"
  if(is.null(data$type)) data$type <- 1
  data$type[is.na(data$type)] <- -1
  
  
  # NODES (GET UNIQUE NUMERICAL NODE NAMES)
  node_names <- unique(data$node)
  node_number <- 1:length(node_names)
  lt <- data.frame(node_names, node_number, stringsAsFactors = FALSE)
  data <- merge(data, lt, by.x = "node", by.y = "node_names", all.x = TRUE)
  data$node <- data$node_number
  data$node_number <- NULL
  arrows <- merge(arrows, lt, by.x = "to", by.y = "node_names")
  arrows$to <- arrows$node_number
  arrows$node_number <- NULL
  arrows <- merge(arrows, lt, by.x = "from", by.y = "node_names")
  arrows$from <- arrows$node_number
  arrows$node_number <- NULL
  
  
  # Calculate Denominators
  # denominator <- rep(NA, nrow(data))
  # for( i in 1:length(denominator)){
  #  denominator[i] <- sum(data[data[,'node'] %in% eval(parse(text=data[i,"sum_node"])),"N"])
  # }
  
  # Create labels of the form n/N PP%
  # data[,'label'] <- ifelse(
  #  denominator==0,
  #  paste(data[,"label"],"\n",data[,"N"],"/",denominator,sep=""),
  #  paste(data[,"label"],"\n",data[,"N"],"/",denominator," ",sprintf("%3.0f",data[,"N"]/denominator*100),"%",sep="")
  #  )
  
  data[,'label'] <- data[,'label'] %|% "\n" %|% data[,'N']
  
  # Start Plot
  xlim <- c(-1,1)
  ylim <- c( 0,1)
  par(mar=rep(0,4), oma=c(.5,.5,1,.5))
  plot.new()
  plot.window(xlim=c(-1,1), ylim=c(0,1), xaxs="i", yaxs="i")

  # BOX WIDTH AND HEIGHT SCHEMES (TYPE)
  for(j in unique(data$type)){
    data$box_width[data$type == j] <- max(strwidth(data$label[data$type == j]) + strwidth("M"))
    data$box_height[data$type == j] <- max(strheight(data$label[data$type == j]) + strheight("M"))
  }
  
  
  # Calculate box centers
  data <- partition2(xlim=c(-1,1), ylim=c(0,1), data)
  
  # Array for box edges
  box_edges <- array(NA, dim=c(nrow(data),5*2 + 1))
  box_edges[,1] <- data[,'node']
  
  # Draw boxes
  for(i in 1:nrow(data)){
    edges <- draw_box(
      data$label[i],
      data$col_centers[i],
      data$row_centers[i],
      data$box_width[i],
      data$box_height[i],
      plot = data$plot[i],
      col = data$color[i],
      border = 1
    )
    box_edges[i,2:9] <- if(data[i,"plot"]){
      c(t(edges))
    }else{
      rep(NA,8)
    }
    box_edges[i,10:11] <- unlist(data[i,c("col_centers","row_centers")])
  }
  
  draw_arraw(arrows, data, box_edges)
  
  box()
}

draw_arraw <- function(arrows,box_centers,box_edges){
  suppressPackageStartupMessages(require(showtext))
  for(i in 1:nrow(arrows)){
    
    same_row <- box_centers[box_centers[,'node'] == arrows[i,'from'],'row'] == box_centers[box_centers[,'node'] == arrows[i,'to'],'row']
    
    if(same_row){
      eee <- rbind(
        t(matrix(unname(unlist(box_edges[arrows[i,'from'], -1])),nrow=2)),
        t(matrix(unname(unlist(box_edges[arrows[i,'to'], -1])),nrow=2))
      )
      ddd <- as.matrix(dist(eee))[6:10,1:5]
      ddd[is.na(ddd)] <- Inf
      shortest <- which(ddd == min(ddd), arr.ind = TRUE)
      to_from <- eee[shortest+c(5,0),]
      lines(to_from,lwd=2)
      showtext.begin()
      # Left or right arrow
      if(to_from[1,1] > to_from[2, 1]) text(eee[shortest[1]+5,1],eee[shortest[1]+5,2],"\u25BA",cex=1.5)
      if(to_from[1,1] < to_from[2, 1]) text(eee[shortest[1]+5,1],eee[shortest[1]+5,2],"\u25C4",cex=1.5)
      showtext.end()
    }else{
      
      ll <- rbind(
        unname(unlist(box_edges[box_edges[,1] == arrows[i,'from'],1 + 1:2])),
        unname(unlist(box_edges[box_edges[,1] == arrows[i,'to'],  1 + 5:6]))
      )
      
      phi <- 1 - (-1+sqrt(5))/2
      
      #line1
      l1 <- ll
      l1[2,1] <- l1[1,1]
      l1[2,2] <- l1[2,2]*phi + (1-phi)*l1[1,2]
      lines(l1,lwd=2)
      
      #line2
      l2 <- ll
      l2[1,1] <- l2[2,1]
      l2[1,2] <- l1[2,2]
      l2[2,2] <- l2[2,2] + strheight("\u25BC",cex=1.5)/2
      lines(l2,lwd=2)
      
      #line3
      lines(rbind(l1[2,],l2[1,]),lwd=2)
      
      #triangle
      R <- c(0,1,-1,0)
      dim(R) <- c(2,2)
      showtext.begin()
      text(l2[2,1],l2[2,2],"\u25BC",cex=1.5)
      showtext.end()
    }
  }
}

partition2 <- function(xlim,ylim,data){
  
  nspaces <- length(unique(data$row))
  cushion_space <- strheight("M\nM")/(sum(data[!duplicated(data$row),'box_height'])+nspaces*strheight("M\nM"))
  bh <- (data[!duplicated(data$row),'box_height']) /(sum(data[!duplicated(data$row),'box_height'])+nspaces*strheight("M\nM"))+cushion_space
  data$height_row <- 0
  data[!duplicated(data$row),'height_row'] <- (ylim[2] - ylim[1])*bh
  for( i in unique(data$row)){
    data[data$row==i,'height_row'] <- max(data[data$row==i,'height_row'])
  }
  
  data[,'row_centers'] <- 0
  for( i in sort(unique(data$row),decreasing=TRUE)){
    data[data$row==i,'row_centers'] <- sum(data[data$row > i & !duplicated(data$row),'height_row']) + data[data$row == i & !duplicated(data$row),'height_row']/2 + ylim[1]
  }
  
  data[,'col_centers'] <- 0
  width <- xlim[2] - xlim[1]
  for( i in unique(data$row) ){
    col_width <- width /  max(data[data$row==i,'col'])
    data[data$row==i,'col_centers'] <-  -col_width/2 + col_width*(data[data$row==i,'col']) + xlim[1]
  }
  
  return(data)
}

draw_box <- function(text,x,y,box_width,box_height,plot=TRUE,...){
  x1 <- x - box_width/2
  y1 <- y - box_height/2
  x2 <- x + box_width/2
  y2 <- y + box_height/2
  
  if(plot){
    rect(x1,y1,x2,y2,...)
    text(x,y,text)
  }
  
  out <- rbind(
    c(x,y1),
    c(x1,y),
    c(x,y2),
    c(x2,y)
  )
  return(out)
}

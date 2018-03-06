### Make wordtree using google's wordtree function


#' @title wordtree
#' @description This is a simple wrapper for google's awesome Wordtree function
#' https://developers.google.com/chart/interactive/docs/gallery/wordtree. NOTE: You _do_ need to be online to use this
#' package, although apparently no data is sent to servers, as per
#' https://developers.google.com/chart/interactive/docs/gallery/wordtree#data-policy.
#' @param text A vector of phrases or sentances that should be used for the wordnet. You can feed the raw inputs in, we
#' will not use the entire sentance but will extract what we want using the `Number_words` parameter. I DO throw out all
#' punctuation, numbers and characters since what we're interested in is the words.
#' @param targetWord This is the main word which should be the "root" of the tree
#' @param direction This specifies in what direction the tree should "grow". To have the root word on the left, and the
#' tree on the right, type "suffix". In a "prefix" word tree, the root is on the right, and in a "double" word tree,
#' it's in the center.
#' @param Number_words This specifies how many words you would like the "tree" to consist of, not including the root. IE,
#' if we feed the wordtree two sentences, "I like the man known as Ike", and "Hey, what's up Ike?" and select `Number_words`
#' =2, then the sentances will be shortened to "known as Ike" and "what's up Ike"
#' @param fileName If you would like the wordtree to be saved somewhere, then specify the filename here.
#' @return returns an html file containing the wordtree.
#' @examples
#' sample <- c("cats are better than dogs",
#' "cats eat kibble",
#' "cats are better than hamsters",
#' "cats are awesome",
#' "cats are people too",
#' "cats eat mice",
#' "cats meowing",
#' "cats in the cradle",
#' "cats eat mice",
#' "cats in the cradle lyrics",
#' "cats eat kibble",
#' "cats for adoption",
#' "cats are family",
#' "cats eat mice",
#' "cats are better than kittens",
#' "cats are evil",
#' "cats are weird",
#' "cats eat mice")
#'
#' wordtree(text=sample,targetWord = "cats",direction="suffix",Number_words = 4,fileName="thingie.html")
#' browseURL("thingie.html")
#'
#' wordtree(text=sample,targetWord = "are",direction="double",Number_words=3,fileName="thingie.html")
#' browseURL("thingie.html")
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_extract}}
#' @rdname wordtree
#' @export
#' @importFrom stringr str_extract
wordtree <- function(text,targetWord,direction,Number_words,fileName){
  Number_words <- Number_words -1
  text <- gsub(pattern = "[^a-zA-Z ]","",x=text)
  text <- gsub(pattern = "http[a-zA-Z]+","",x=text,perl = T)

  S <- "( ?\\w+ ?)"

  if(direction=="double"){
    RE <- paste0("(\\S+\\s+|^)",
                 paste(rep(S,Number_words),collapse=""),
                 targetWord,
                 paste(rep(S,Number_words),collapse=""),
                 "(\\s+\\S+|$)")
  } else if(direction=="suffix"){
    RE <- paste0(
      targetWord,paste(rep(S,Number_words),collapse=""),
      "(\\s+\\S+|$)")

  } else if(direction=="prefix"){
    RE <- paste0("(\\S+\\s+|^)",paste(rep(S,Number_words),collapse=""),targetWord)

  }

  x <- stringr::str_extract(text,RE)
  x <- x[!is.na(x)]
  x <- paste(x,collapse="'],['")
  x <- paste0("['",x,"']")

  top= "<html>
  <head>
  <script type=\"text/javascript\" src=\"https://www.gstatic.com/charts/loader.js\"></script>
  <script type=\"text/javascript\">
  google.charts.load('current', {packages:['wordtree']});
  google.charts.setOnLoadCallback(drawChart);

  function drawChart() {
  var data = google.visualization.arrayToDataTable(
  [ ['Phrases'],"

  bottom <- paste0("]
  );

                   var options = {
                   wordtree: {
                   format: 'implicit',
                   word: '",targetWord,"',
                   type: '",direction,"'
                   }
                   };

                   var chart = new google.visualization.WordTree(document.getElementById('wordtree_basic'));
                   chart.draw(data, options);
  }
                   </script>
                   </head>
                   <body>
                   <div id=\"wordtree_basic\" style=\"width: 900px; height: 500px;\"></div>
                   </body>
                   </html>")

  cat(top,x,bottom,file=fileName)
}


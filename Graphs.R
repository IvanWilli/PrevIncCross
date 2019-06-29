#http://rich-iannone.github.io/DiagrammeR/graphviz_and_mermaid.html
library('DiagrammeR')
grViz(
  diagram="
      digraph bboxes_and_circles{
      graph [overlap = true, fontsize = 10]
      node [shape = circle,
              fixedsize = true,
              width = 0.9]
      H->D; HTA->D [color = blue] ; H->H; HTA->HTA [color = blue]
      {rank=same ; H -> HTA [color = blue]};
       } ")




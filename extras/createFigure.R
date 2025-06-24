# install.packages("DiagrammeR")   # once
library(DiagrammeR)
library(DiagrammeRsvg)
library(grid)

dot_code <- "
digraph FedNestedCV {

  /* ---------- general graph attributes --------------------------- */
  graph [rankdir = TB, nodesep = 0.4, bgcolor = white]

  /* ---------- colour palette ------------------------------------- */
  /* cylinders:                 training  = blue
                                validation = yellow
                                outer test = orange               */
  /* server box:                green                               */
  /* arrows & frames:           blue                                */

  /* ========== ITERATION 1 : inner-fold 1 ========================= */
  subgraph cluster_iter1 {

      label = \"Inner fold 1\";
      fontname = Helvetica;
      style = dashed;
      color = \"#6C8EBF55\" ;     /* faint blue frame               */

      /* --- clients ------------------------------------------------ */
      node [shape = cylinder, style = filled, width = 0.9,
            height = 0.6, fontname = Helvetica, fontsize = 10,
            fillcolor = \"#BDD7EE\"]                   /* training  */

      T1_1  [label = \"Fold 1\"]
      T2_1  [label = \"Fold 2\"]
      T3_1  [label = \"Fold 3\"]
      
      node [fillcolor = \"#FFD966\"]                  /* validation */
      V_1   [label = \"Fold 4\\n(validation)\"]
      
      node [fillcolor = \"#F6B26B\"]                  /* outer test */
      Test  [label = \"Fold 5\\n(outer test)\"]
      
      /* --- server ------------------------------------------------- */
      node [shape = box, style = filled, fillcolor = \"#C6E0B4\",
            width = 1.4, height = 0.8, fontname = Helvetica,
            fontsize = 11]
      S1    [label = \"Server\"]
      
      /* --- communication arrows ---------------------------------- */
      edge  [arrowhead = none, dir = both,
             penwidth = 1.4, color = \"#6C8EBF\"]      /* solid CTR  */
      T1_1 -> S1
      T2_1 -> S1
      T3_1 -> S1
      
      edge  [style = dashed, dir = forward, arrowhead = normal,
             penwidth = 1.4, color = \"#6C8EBF\"]      /* download   */
      S1   -> V_1
      S1   -> Test
      
     T1_1 -> T2_1 [style = invis]
     T2_1 -> T3_1 [style = invis]                                                                           
     T3_1 -> V_1  [style = invis]                                                                         
     V_1  -> Test [style = invis]                          
  }
  
  /* ========== ELLIPSIS  ========================================== */
  node  [shape = plaintext, label = \"...\"] Ellipsis

  /* ========== ITERATION 2 : inner-fold 2 ========================= */
  subgraph cluster_iter2 {

      label = \"Inner fold 2\";
      fontname = Helvetica;
      style = dashed;
      color = \"#6C8EBF55\";
      
      node [shape = cylinder, style = filled, width = 0.9,
            height = 0.6, fontname = Helvetica, fontsize = 10,
            fillcolor = \"#BDD7EE\"]
      T1_2  [label = \"Fold 1\"]
      T2_2  [label = \"Fold 2\"]
      T4_2  [label = \"Fold 4\"]
      
      node [fillcolor = \"#FFD966\"]
      V_2   [label = \"Fold 3\\n(validation)\"]
      
      node [fillcolor = \"#F6B26B\"]
      Test2 [label = \"Fold 5\\n(outer test)\"]
      
      
      node [shape = box, style = filled, fillcolor = \"#C6E0B4\",
            width = 1.4, height = 0.8, fontname = Helvetica,
            fontsize = 11]
      S2    [label = \"Server\"]
      
      edge  [arrowhead = none, dir = both,
             penwidth = 1.4, color = \"#6C8EBF\"]
      T1_2 -> S2
      T2_2 -> S2
      T4_2 -> S2
      
      edge  [style = dashed, dir = forward, arrowhead = normal,
             penwidth = 1.4, color = \"#6C8EBF\"]
      S2   -> V_2
      S2   -> Test2
      T1_2 -> T2_2 [style = invis]
      T2_2 -> V_2  [style = invis]
      V_2  -> T4_2 [style = invis]
      T4_2 -> Test2 [style = invis]
  }

  /* ---------- align the two clusters on the same rank ------------ */
  { rank = same; S1 Ellipsis S2 }

  /* ========== LEGEND ============================================= */
  subgraph cluster_legend {

      label = \"Legend\";
      fontname = Helvetica;
      style = invis;                          /* no surrounding box  */
      rankdir = LR;                           /* arrows left→right      */
      rank = sink;                            /* push legend to bottom  */

      /* two silent points to draw a solid double arrow --------------*/
      node [shape = point, width = 0.01, label = \"\"]
      n1; n2;

      /* two silent points to draw a dashed single arrow -------------*/
      n3; n4;

      /* solid, double-headed arrow : model-update vector ------------*/
      edge  [arrowhead = none, dir = both,
             penwidth = 1.4, color = \"#6C8EBF\"]
      n1 -> n2 [xlabel = \"update vector\" fontsize = 8]

      /* dashed, single-headed arrow : evaluation metric -------------*/
      edge  [style = dashed, dir = forward, arrowhead = normal,
             penwidth = 1.4, color = \"#6C8EBF\"]
      n3 -> n4 [xlabel = \"AUC only\" fontsize = 8]

      /* invisible link to keep the two arrows vertically separated */
      n2 -> n3 [style = invis]
  }

  /* ---------- global title --------------------------------------- */
  labelloc = \"t\"
  label = \"Federated nested 5-fold cross-validation\\n
           (outer test = Fold 5; validation rotates among the 4 training clients)\"
}
"

## build widget and capture as grob ---------------------------------
widget <- grViz(dot_code)
svg_txt <- export_svg(widget)
writeLines(svg_txt, "nested_cv_diagram.svg")

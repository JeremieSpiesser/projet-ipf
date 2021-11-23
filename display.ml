module IntSet = Set.Make(struct type t = int let compare = (-) end)


let draw_edge ((x1,y1),(x2,y2)) =
    let _ = Graphics.moveto x1 y1 in
    Graphics.lineto x2 y2

let draw_pb pts =
    let c = Graphics.foreground in
    let _ = Graphics.set_color Graphics.black in
    let _ = List.iter (fun (x,y) -> Graphics.fill_circle x y 5) pts in
    Graphics.set_color c

let draw_grid sol =
    let xs =
        IntSet.elements
            (List.fold_left
                (fun acc ((x1,_),(x2,_)) -> IntSet.add x1 (IntSet.add x2 acc)
                ) IntSet.empty sol
            )
    in
    let ys =
        IntSet.elements
            (List.fold_left
                (fun acc ((_,y1),(_,y2)) -> IntSet.add y1 (IntSet.add y2 acc)
                ) IntSet.empty sol
            )
    in
    let xm = List.hd xs in
    let xM = List.hd (List.rev xs) in
    let ym = List.hd ys in
    let yM = List.hd (List.rev ys) in
    let c = Graphics.foreground in
    let _ = Graphics.set_color Graphics.black in
    let _ = List.iter (fun x -> draw_edge ((x,ym), (x,yM))) xs in
    let _ = List.iter (fun y -> draw_edge ((xm,y), (xM,y))) ys in
    Graphics.set_color c

let draw_sol sol =
    let c = Graphics.foreground in
    let _ = Graphics.set_color Graphics.red in
    let _ = Graphics.set_line_width 3 in
    let _ = List.iter draw_edge sol in
    let _ = Graphics.set_line_width 1 in
    Graphics.set_color c

let draw (sx,sy) pts sol grid =
    let args = Printf.sprintf " %dx%d" sx sy in
    let _ = Graphics.open_graph args in
    let _ = draw_pb pts in
    let _ = if grid then draw_grid sol else () in
    let _ = draw_sol sol in
    let _ = Graphics.wait_next_event [Graphics.Key_pressed] in
    Graphics.close_graph ()


let draw_rectilinear (sx,sy) pts sol =
    let ps  = List.fold_left (fun acc (c1,c2) -> c1::c2::acc) pts sol in
    let xm  = List.fold_left (fun acc (x,_) -> min acc x) max_int ps in
    let xM  = List.fold_left (fun acc (x,_) -> max acc x) min_int ps in
    let ym  = List.fold_left (fun acc (_,y) -> min acc y) max_int ps in
    let yM  = List.fold_left (fun acc (_,y) -> max acc y) min_int ps in
    let margin  = 10 in
    let _ = if sx <= 2 * margin
            then failwith "[draw_rectilinear] width must be more than 20px."
            else ()
    in
    let _ = if sy <= 2 * margin
            then failwith "[draw_rectilinear] height must be more than 20px."
            else ()
    in
    let adjust (x,y) =
        ( margin + (x-xm) * (sx - 2*margin) / max 1 (xM - xm) 
        , margin + (y-ym) * (sy - 2*margin) / max 1 (yM - ym) 
        )
    in
    let pts' = List.map adjust pts in
    let sol' = List.map (fun (c1,c2) -> (adjust c1, adjust c2)) sol in
    draw (sx,sy) pts' sol' true

let draw_eulcidean (sx,sy) pts sol =
    let ps0 = List.map (fun (x,y) -> (float_of_int x, float_of_int y)) pts in
    let ps  = List.fold_left (fun acc (c1,c2) -> c1::c2::acc) ps0 sol in
    let xm  = List.fold_left (fun acc (x,_) -> min acc x) infinity ps in
    let xM  = List.fold_left (fun acc (x,_) -> max acc x) neg_infinity ps in
    let ym  = List.fold_left (fun acc (_,y) -> min acc y) infinity ps in
    let yM  = List.fold_left (fun acc (_,y) -> max acc y) neg_infinity ps in
    let margin  = 10 in
    let _ = if sx <= 2 * margin
            then failwith "[draw_euclidean] width must be more than 20px."
            else ()
    in
    let _ = if sy <= 2 * margin
            then failwith "[draw_euclidean] height must be more than 20px."
            else ()
    in
    let shift_x = float_of_int margin -. xm in
    let shift_y = float_of_int margin -. ym in
    let eps     = 1e-6 in
    let zoom_x  = max eps (float_of_int (sx - 2 * margin) /. (max eps (xM -. xm))) in
    let zoom_y  = max eps (float_of_int (sy - 2 * margin) /. (max eps (yM -. ym))) in
    let adjust (x,y) =
        ( int_of_float (shift_x +. zoom_x *. x)
        , int_of_float (shift_y +. zoom_y *. y)
        )
    in
    let pts' = List.map adjust ps0 in
    let sol' = List.map (fun (c1,c2) -> (adjust c1, adjust c2)) sol in
    draw (sx,sy) pts' sol' false


(*
let _ = draw_rectilinear
            (800, 600)
            [(0,0); (3,4); (4,1)] 
            [((0,0),(0,1)); ((0,1),(3,1)); ((3,1),(3,4)); ((3,1),(4,1))]

let _ = draw_eulcidean
            (800, 600)
            [(0,0); (3,4); (4,1)] 
            [((0.,0.),(2.5,1.5)); ((3.,4.),(2.5,1.5)); ((4.,1.),(2.5,1.5))]
*)

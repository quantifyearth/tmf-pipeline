let template title body =
  let open Htmlit in
  let more_head =
    El.splice
      [
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/base-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/grids-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/grids-responsive-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/buttons-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href
                "https://cdn.jsdelivr.net/npm/purecss@3.0.0/build/tables-min.css";
            ]
          ();
        El.link
          ~at:
            [
              At.rel "stylesheet";
              At.href "https://unpkg.com/leaflet@1.9.4/dist/leaflet.css";
            ]
          ();
        El.script
          ~at:[ At.src "https://unpkg.com/leaflet@1.9.4/dist/leaflet.js" ]
          [];
        El.style
          [
            El.unsafe_raw
              {|
        body {
            margin: 24px
        }

        .l-box {
            padding: 0em 2em;
        }

        .pure-g .pure-u-3-5 div {
            border-right: thin solid grey;
        }

        #map { width: 100%; height: 400px }
    |};
          ];
      ]
  in
  El.page ~lang:"en" ~more_head ~title body

let divc class' children =
  Htmlit.El.div ~at:[ Htmlit.At.class' class' ] children

let pure_button ?(disabled = false) href txt =
  Htmlit.El.a
    ~at:
      [
        Htmlit.At.href href;
        (if disabled then Htmlit.At.disabled else Htmlit.At.void);
        Htmlit.At.class' "pure-button";
      ]
    [ Htmlit.El.txt txt ]

let map geojsons =
  let open Htmlit in
  let add i data =
    let set_view =
      if i = 0 then Fmt.str "map.setView(k.getBounds().getCenter(), 6)" else ""
    in
    Fmt.str
      {|fetch("%s").then(res => res.json()).then(data => {
    // add GeoJSON layer to the map once the file is loaded
    var k = L.geoJson(data).addTo(map);
    %s
    });
    |}
      data set_view
  in
  El.script
    ([
       El.unsafe_raw
         {|
      var map = L.map('map').setView([54.505, -6], 9);
      L.tileLayer('https://tile.openstreetmap.org/{z}/{x}/{y}.png', {
          attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
      }).addTo(map);
    |};
     ]
    @ List.mapi (fun i v -> El.unsafe_raw (add i v)) geojsons)

module Table = struct
  type t = string list list

  let render t =
    let open Htmlit in
    let row ?(header = false) s =
      let el = if header then El.th else El.td in
      El.tr (List.map (fun s -> el [ El.txt s ]) s)
    in
    match t with
    | header :: data ->
        let header = El.thead [ row ~header:true header ] in
        El.table
          ~at:[ At.class' "pure-table" ]
          [ header; El.tbody (List.map (fun r -> row r) data) ]
    | _ -> El.txt "Something went wrong rendering the tabluar data"
end

let page ?(geojsons = []) ?(images = []) ?(tabular = []) ~title ~id ~inputs
    ~manifest () =
  let open Htmlit in
  El.splice
    [
      divc "pure-g"
        [
          divc "pure-u-3-5"
            [
              divc "l-box"
                [
                  El.h2 [ El.txt "Data and Build Information" ];
                  El.p
                    [
                      El.a
                        ~at:[ At.href ("/alt/job/" ^ id) ]
                        [ El.txt Fmt.(str "Old page with full log") ];
                    ];
                  El.em [ El.txt ("Job ID: " ^ id) ];
                  El.p
                    [
                      pure_button "#" "Download Data";
                      pure_button ~disabled:true "#" "Run Shell";
                      pure_button ~disabled:true "#" "Run Notebook";
                    ];
                  El.p
                    [
                      El.txt
                        "The following files were generated during this build \
                         step. Clicking 'Download Data' will zip these up and \
                         begin a download for them. This is only really \
                         feasible on relatively small datasets.";
                    ];
                  El.pre [ El.code [ El.txt manifest ] ];
                  (match geojsons with
                  | [] -> El.void
                  | _ ->
                      El.splice
                        [
                          El.h2 [ El.txt "Maps" ];
                          El.p
                            [
                              El.txt
                                "The map below plots the obviously plottable \
                                 pieces of data that we could find. For now \
                                 this is any GeoJSON data files.";
                            ];
                          El.p
                            [
                              El.em
                                [
                                  El.txt
                                    "Note we've zoomed into bounding box of \
                                     the first piece of data, the map may \
                                     contain others!";
                                ];
                            ];
                          El.div ~at:[ At.id "map" ] [];
                        ]);
                  (match images with
                  | [] -> El.void
                  | urls ->
                      El.splice
                        ([
                           El.h2 [ El.txt "Images" ];
                           El.p
                             [
                               El.txt
                                 "Images generated as output data, we only \
                                  render PNG or JPEG images for now as they \
                                  are likely to be small enough.";
                             ];
                         ]
                        @ List.map
                            (fun url ->
                              El.img ~at:[ At.v "width" "100%"; At.src url ] ())
                            urls));
                  (match tabular with
                  | [] -> El.void
                  | data :: _ ->
                      El.splice
                        [
                          El.h2 [ El.txt "Tabular Data" ];
                          El.p
                            [
                              El.txt
                                "Tables for any CSV files found in the output \
                                 data directory.";
                            ];
                          Table.render data;
                        ]);
                  El.h2 [ El.txt "Summary Statistics" ];
                  El.p [ El.em [ El.txt "Coming soon..." ] ];
                ];
            ];
          divc "pure-u-2-5"
            [
              divc "l-box"
                [
                  El.h3 [ El.txt "Build Summary" ];
                  El.p
                    [
                      El.txt
                        "The following command was the last to be run in this \
                         pipeline step.";
                    ];
                  El.pre
                    [
                      El.code [ El.txt "python -m methods.matching.find_pairs" ];
                    ];
                  El.h3 [ El.txt "Data Dependencies" ];
                  El.p
                    [
                      El.txt
                        {|
              The following list is the immediate data dependencies of this build step.
              Put another way, these build outputs were made available to this build step
              in read-only mode. The actual code may or may not have used the data.
            |};
                    ];
                  El.ul
                    (List.map
                       (fun i ->
                         El.li
                           [ El.a ~at:[ At.href "#" ] [ El.code [ El.txt i ] ] ])
                       inputs);
                  El.h3 [ El.txt "Build Specifications" ];
                  El.p
                    [
                      El.txt
                        "The build specification is a bit like a Dockerfile. \
                         In this form it is very raw and specific to how the \
                         dataflow pipeline works, but it has been copied here \
                         for your convenience.";
                    ];
                  El.pre
                    [
                      El.code
                        [
                          El.txt
                            {|((from alpine) (run (shell "echo 'hello world'")))|};
                        ];
                    ];
                  El.h3 [ El.txt "Logs" ];
                  El.p
                    [
                      El.txt
                        {|Raw logs from the build of this particular pipeline step. The data here is very raw, but can help explain how the data was produced.|};
                    ];
                  pure_button "./logs" "Raw Logs";
                ];
            ];
        ];
      (match geojsons with [] -> El.void | lst -> map lst);
    ]
  |> template title

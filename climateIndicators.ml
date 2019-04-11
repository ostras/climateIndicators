(*
Author: Bruno Sousa  /adaptado por Ricardo Baptista
Version: v1.0 2017/11/27
Description: Programa ilustrativo da abertura e leitura de ficheiros em OCaml

*)

let file_temp = Sys.argv.(1);;
let file_precip = Sys.argv.(2);;

(* Indicacao do nome dos ficheiros a ler 
let file_temp = "files_aux/temperaturas.txt";;
let file_precip = "files_aux/precipitacoes.txt";;*)


let split_on_char sep s =
  let r = ref [] in
  let j = ref (String.length s) in
  for i = String.length s - 1 downto 0 do
    if s.[i] = sep then begin
      r := String.sub s (i + 1) (!j - i - 1) :: !r;
      j := i
    end
  done;
  String.sub s 0 !j :: !r
;;

(*
Description: Função recursiva para a abertura e leitura de um ficheiro
Input: name - Nome do ficheiro
Output: List de strings com o conteúdo do ficheiro
*)
let read_lines name : string list =
  if Sys.file_exists (name) then
    begin
      let ic = open_in name in
        try
          let try_read () =
            try Some (input_line ic) with End_of_file -> None in
          let rec loop acc = match try_read () with
            | Some s -> loop (s :: acc)
            | None -> close_in_noerr ic; List.rev acc in
            loop []
        with e ->
          close_in_noerr ic;
          []
    end
  else
    []
;;

(* Leitura do conteúdo dos ficheiros *)
let temperaturas = read_lines file_temp;;
let precipitacoes = read_lines file_precip;;

let () =
  for i = 0 to (List.length temperaturas) - 1 do
    (*Cada linha do ficheiro temperaturas*)
    let linhaTemps = (List.nth temperaturas i) in
    let linhaSplitTemps = split_on_char ';' linhaTemps in
    (*Cada linha do ficheiro precipitacoes*)
    let linhaPrec = (List.nth precipitacoes i) in
    let linhaSplitPrec = split_on_char ';' linhaPrec in
    (*Variáveis auxiliares de médias, míminas, máximas, e auxiliares para os indicadores*)
    let mediaTemp = ref 0.0 in
    let mediaPrec = ref 0.0 in
    let maxTemp = ref (-200.0) in
    let minTemp = ref 200.0 in
    let precPosAnual = ref 0.0 in
    let tempPosAnual = ref 0.0 in
    (*Ciclo principal - assume-se que o número de linhas e ordem de cada localidade e ano é consistente entre os 2 ficheiros*)
    for j = 0 to (List.length linhaSplitTemps) - 1 do
      let subLinhaTemp = (List.nth linhaSplitTemps j) in
      (*Primeiro item de cada linha - cidade*)
      if j = 0 then     
        Printf.printf "Cidade : %s " subLinhaTemp
      (*Segundo item de cada linha - ano*)
      else if j = 1 then
        Printf.printf "Ano : %s\n" subLinhaTemp
      (*Restantes items - valores de temperaturas e precipitacoes*)
      else if j > 2 && j < 15 then
        begin
        let valor = split_on_char ':' subLinhaTemp in
        let elem = List.nth valor 1 in
        if elem <> "n/d" then
          let valorFloatTemp = float_of_string elem in
          mediaTemp := !mediaTemp +. valorFloatTemp;
        if valorFloatTemp > !maxTemp then maxTemp := valorFloatTemp;
        if valorFloatTemp < !minTemp then minTemp := valorFloatTemp;

        let subLinhaPrec = (List.nth linhaSplitPrec j) in
        let valorPrec = split_on_char ':' subLinhaPrec in
        let elemPrec = List.nth valorPrec 1 in
        if elemPrec <> "n/d" then
          let valorFloatPrec = float_of_string elemPrec in
          mediaPrec := !mediaPrec +. valorFloatPrec;
        (*Cálculos auxiliares Indicador 2*)
        if valorFloatTemp >= 0.0 then
          begin
            tempPosAnual := !tempPosAnual +. valorFloatTemp;
            precPosAnual := !precPosAnual +. valorFloatPrec;
          end
        end
    done;
    (*Temperatura média anual*)
    mediaTemp := !mediaTemp /. 12.0;
    Printf.printf "Temp. Media Anual : %f\n" !mediaTemp;
    (*Precipitacao média anual*)
    mediaPrec := !mediaPrec /. 12.0;
    Printf.printf "Precip. Media Anual : %f\n" !mediaPrec;
    (*Indicador 1*)
    let indicadorIC = !maxTemp -. !minTemp in
    Printf.printf "Indicador 1 : %f\n" indicadorIC;
    (*Indicador 2*)
    let indicadorIo = 10.0 *. (!precPosAnual /. !tempPosAnual) in
    Printf.printf "Indicador 2 : %f\n\n" indicadorIo;
done;;


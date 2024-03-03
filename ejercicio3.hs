import Data.Char (toLower, isLetter, ord, chr)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

-- Define el alfabeto estándar y el alfabeto cifrado Atbash
let alphabet = ['a'..'z']
let atbashAlphabet = reverse alphabet

-- Función para encriptar un caracter
let encryptChar c = if isLetter c then atbashAlphabet !! fromJust (elemIndex (toLower c) alphabet) else c

-- Función para encriptar un mensaje
let atbashEncrypt msg = map encryptChar msg

-- Función para desencriptar un mensaje (es lo mismo que encriptar con Atbash)
let atbashDecrypt = atbashEncrypt

-- Función para formatear un mensaje en grupos de 5 caracteres
let formatOutput msg = unwords . takeWhile (not . null) . map (take 5) . iterate (drop 5) $ msg

-- Ejemplo de uso
let encoded = formatOutput $ atbashEncrypt "thequickbrownfoxjumpsoverthelazydog"
let decoded = atbashDecrypt "gsvjfrxpyildmulcfqfnkhlevigsvozabwlt"
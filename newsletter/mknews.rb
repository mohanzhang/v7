# encoding: utf-8

# This script generates a webpage that allows readers to decrypt an encrypted
# newsletter. The newsletter is input using textile, and rendered in HTML using
# homoglyphs where possible.
#
# Example usage:
# ruby mknews.rb 2013_june.markdown

require 'rubygems'
require 'rdiscount'
require 'erb'

HOMOGLYPHS = {
  "A" => "\u0410",
  "B" => "\u0412",
  "C" => "\u0421",
  "E" => "\u0415",
  "H" => "\u041D",
  "I" => "\u0406",
  "K" => "\u212A",
  "L" => "\u216C",
  "M" => "\u041C",
  "O" => "\u041E",
  "P" => "\u0420",
  "S" => "\u0405",
  "T" => "\u03A4",
  "V" => "\u2164",
  "X" => "\u2169",
  "a" => "\u0430",
  "c" => "\u217D",
  "d" => "\u217E",
  "e" => "\u0435",
  "i" => "\u0456",
  "j" => "\u0458",
  "m" => "\u217F",
  "p" => "\u0440",
  "v" => "\u2174",
  "x" => "\u2179"
}

def sub_glyphs(input)
  input.split(/ /).map {|word|
    unless word.include?("http") || word.include?("_") || word.include?("'")
      HOMOGLYPHS.each do |k,v|
        word.gsub!(k,v)
      end
    end

    word
  }.join(' ')
end

infile = ARGV.shift
input = IO.read(infile)
template = IO.read("template.html.erb")

TMP_PROCESSED_FILE = "temp_processed.html"
TMP_CIPHERED = "temp_ciphered"

glyphed_input = sub_glyphs(input)
markdown = RDiscount.new(input, :smart, :autolink)
rendered_input = markdown.to_html

File.open(TMP_PROCESSED_FILE, "w") {|f| f.write rendered_input}

# encrypt using openssl

`openssl enc -aes-256-cbc -in #{TMP_PROCESSED_FILE} -out #{TMP_CIPHERED} -pass pass:"EFFismyBFF" -e -base64`
@cipher64 = IO.read(TMP_CIPHERED)

# Write to output
renderer = ERB.new(template)

File.open(File.basename(infile, ".markdown") + ".html", "w:utf-8") {|f| f.write renderer.result}

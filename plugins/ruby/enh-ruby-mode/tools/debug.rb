#!/usr/bin/env ruby -w

require_relative '../ruby/erm_buffer'

trace = ARGV.delete "--trace"
debug = ARGV.delete "-d"

class ErmBuffer::Parser
  alias :old_realadd :realadd
  def realadd(sym,tok,len)
    x = old_realadd(sym, tok, len)
    k = sym =~ /^rem_/ ? :rem : sym
    v = ErmBuffer::FONT_LOCK_NAMES[k] || -1
    puts "%2d %-20p %3d %p" % [v, sym, len, tok]
    x
  end
end

if trace then
  require "tracer"
  Tracer.on
end

ARGV.each do |file|
  buf = ErmBuffer.new
  buf.debug = true if debug
  content = File.read file
  point_min, point_max, pbeg, len = 1, content.size+1, 0, content.size

  buf.add_content :x, point_min, point_max, pbeg, len, content

  puts buf.parse
end

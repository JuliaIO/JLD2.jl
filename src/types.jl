@enum LayoutClass::UInt8 begin
    LC_COMPACT_STORAGE = 0x00
    LC_CONTIGUOUS_STORAGE = 0x01
    LC_CHUNKED_STORAGE = 0x02
    LC_VIRTUAL_STORAGE = 0x03
end

@enum(CharacterSet::UInt8,
      CSET_ASCII,
      CSET_UTF8)
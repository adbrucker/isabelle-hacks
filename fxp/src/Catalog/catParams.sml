




signature CatParams =
   sig
      val O_CATALOG_FILES  : Uri.Uri list Unsynchronized.ref
      val O_PREFER_SOCAT   : bool Unsynchronized.ref
      val O_PREFER_SYSID   : bool Unsynchronized.ref
      val O_PREFER_CATALOG : bool Unsynchronized.ref
      val O_SUPPORT_REMAP  : bool Unsynchronized.ref
      val O_CATALOG_ENC    : Encoding.Encoding Unsynchronized.ref

      val catError : CatError.Position * CatError.CatError -> unit
   end


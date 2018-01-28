! { dg-do run }
  character (kind=kind("a")) :: u
  if (kind(u) /= kind("a")) stop 1
  end

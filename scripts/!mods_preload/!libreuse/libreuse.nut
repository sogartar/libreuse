::libreuse <- { };

::mods_registerMod("libreuse", 0.1, "libreuse");

::libreuse.toStrHelper <- function(o, this_func, maxDepth = 0, depth = 0) {
  if (maxDepth > -1 && depth > maxDepth) {
    return "...";
  }

  local prefix = "";
  for (local i = 0; i < depth; i += 1) {
    prefix += "-";
  }

  local t = typeof(o);
  if (t == "table" || t == "array") {
    local res = "{\n";
    foreach (member, value in o) {
      res += prefix + member + ": " + this_func(value, this_func, maxDepth, depth + 1) + "\n";
    }
    res += prefix + "}";
    return res;
  } else if (t == "string" || t == "float" || t == "integer" || t == "bool") {
    return "" + o;
  } else {
    return t;
  }
};

::libreuse.toStr <- function(o, maxDepth = 0, depth = 0) {
  return toStrHelper(o, toStrHelper, maxDepth, depth);
};

::libreuse.getMember <- function(o, name) {
  while (true) {
    if (name in o) {
      return o[name];
    } else {
      o = o[o.SuperName];
    }
  }
};

::libreuse.getParentClass <- function(c, name) {
  while (true) {
    #local isSuperNameMember = "SuperName" in c;
    #local isNotSuperNameMember = !("SuperName" in c);
    #this.logInfo("isNotSuperNameMember = " + isNotSuperNameMember);
    #this.logInfo("c = " + ::libreuse.toStr(c, 4));
    if (!("SuperName" in c)) {
      return null;
    } else if (name == c.SuperName) {
      return c[name];
    } else {
      c = c[c.SuperName];
    }
  }
};

::libreuse.copyMember <- function(o, name, newName) {
  while (true) {
    if (name in o) {
      o[newName] <- o[name];
      break;
    } else {
      o = o[o.SuperName];
    }
  }
};

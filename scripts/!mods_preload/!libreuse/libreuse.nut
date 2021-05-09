::mods_registerMod("libreuse", 0.3.0, "libreuse");

local gt = this.getroottable();

if (!("libreuse" in gt)) {
  ::libreuse <- {};
}

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

::libreuse.eulerNumber <- 2.718281;

::libreuse.gaussian <- function(x, b=0, c=1) {
  return this.Math.pow(::libreuse.eulerNumber, -(x - b)*(x - b)/(2.0*c*c));
}

::libreuse.sigmuid <- function(x) {
  return  1 / (1 + this.Math.pow(::libreuse.eulerNumber, -x));
}

::libreuse.factorial <- function(n) {
  local res = 1;
  for (local i = 1; i <= n; i += 1) {
    res *= i;
  }
  return res;
}

::libreuse.binomialCoefficient <- function(n, k) {
  return ::libreuse.factorial(n) / (::libreuse.factorial(k) * ::libreuse.factorial(n - k));
}

::libreuse.binomialPdf <- function(n, k, p) {
  return ::libreuse.binomialCoefficient(n, k) * this.Math.pow(p, k) * this.Math.pow(1 - p, n - k);
}

::libreuse.binomialCdf <- function(n, k, p) {
  local res = 0.0;
  local floorK = this.Math.floor(k);
  for (local i = 0; i <= floorK; i += 1) {
    res += ::libreuse.binomialPdf(n, i, p);
  }
  return res;
}

::libreuse.roundRandomWeighted <- function(x) {
  local fraction = this.Math.floor((x - this.Math.floor(x)) * 1000);
  local res = this.Math.floor(x);
  if (fraction > this.Math.rand(1, 1000)) {
    res += 1;
  }
  return res;
}

::libreuse.findPerkConsts <- function(perkId, perks=gt.Const.Perks.Perks) {
  foreach (perksRow in perks) {
    foreach (perk in perksRow) {
      if (perk.ID == perkId) {
        return perk;
      }
    }
  }

  return null;
}

::libreuse.equidistantPiecewiseLinear <- function(valsArr, arg) {
  local i = this.Math.floor(arg);
  local fraction = arg - this.Math.floor(arg);
  return valsArr[i] * (1 - fraction) + valsArr[i + 1] * fraction;
}

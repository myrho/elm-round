Elm.Native.Round = {};
Elm.Native.Round.make = function(localRuntime) {
	localRuntime.Native = localRuntime.Native || {};
	localRuntime.Native.Round = localRuntime.Native.Round || {};
	if (localRuntime.Native.Round.values)
	{
		return localRuntime.Native.Round.values;
	}

	function truncate(n)
	{
    return Math.trunc(n);
	}

	return localRuntime.Native.Round.values = {
		truncate: truncate
  };
};

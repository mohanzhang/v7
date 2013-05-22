var setTimelineHeight = function () {
  var headingHeight = $("#verticalTimeline h4").outerHeight(true);
  var overheadHeight =  headingHeight + $("#verticalTimeline thead").outerHeight(true);
  var badgeHeight = $("#badgeField").height() - 20 - (2*14);
  $("#verticalTimeline table").height(badgeHeight - headingHeight);
  $("#verticalTimeline tbody").height(badgeHeight - overheadHeight);
};

$(window).resize(setTimelineHeight);

setTimelineHeight();

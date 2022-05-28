t=AbsoluteTime[];
img=;
src = img;
(*grayimage = ColorConvert[img,"Grayscale"];*)
white=Closing[src,BoxMatrix[60]];
srcAdjusted=Image[ImageData[src]/ImageData[white]];
components=ComponentMeasurements[ColorNegate@Binarize[srcAdjusted],{"ConvexArea","Mask"}][[All,2]];
largestComponent=Image[SortBy[components,First][[-1,2]]];
mask=FillingTransform[largestComponent];
corner=MorphologicalBinarize[ImageTake[src,{1700,ImageDimensions[src][[2]]-21},{2000,ImageDimensions[src][[1]]-35}],{0.5,0.2}];
(*lengtharray=ComponentMeasurements[EdgeDetect[corner,0.5,0.28],"MinimalBoundingBox"][[1,2]];
HighlightImage[corner,lengtharray];*)
scaleBar=MorphologicalComponents[DeleteBorderComponents@corner];
block =ComponentMeasurements[{scaleBar,corner},"MaskedImage",#Area> 10000&][[1,2]];
scaleperpixel= Quantity[500,"Micrometers"]/ImageDimensions[block][[1]] //N; (*assumes square pixels*)
(*{l,w}=UnitConvert[{ImageDimensions[img][[1]] scaleperpixel,scaleperpixel ImageDimensions[img][[2]]},"Millimeters"];(*image size in mm*)
toothsurface=Quantity[1.6,"Millimeters"] Quantity[2.2,"Millimeters"] ;(*technical surface area*)*)
outterboundry=Dilation[Opening[DeleteSmallComponents[Colorize@MorphologicalComponents[ImageAdjust[DistanceTransform[Blur[mask,2],0.25]],0.05]],40],BoxMatrix[10]];
(*ImageCompose[img,{outterboundry,1}];*)
innerboundry=Dilation[Opening[DeleteSmallComponents[Colorize@MorphologicalComponents[ImageAdjust[DistanceTransform[Blur[src,2],0.25]],0.05]],40],DiskMatrix[20]];
(*ImageCompose[img,{innerboundry,1}];*)
intersection=ImageMultiply[outterboundry,innerboundry];
leftoverarea=UnitConvert[scaleperpixel^2  ComponentMeasurements[intersection,"Area"][[1,2]],("Millimeters")^2];(*Non woren surface in mm^2*)
actualarea = UnitConvert[scaleperpixel^2 ComponentMeasurements[outterboundry,"Area"][[1,2]],("Millimeters")^2];(*Calculated "real" area of visable tooth*)
weararea = actualarea-leftoverarea;(*Wear of tooth in mm^2*)
exportimage = Overlay[{src,HighlightImage[src,{White,MorphologicalPerimeter[ImageSubtract[outterboundry,innerboundry]]}],Style[ToString[First[weararea]] <> " mm^2",White]},Alignment->Right];
Export["tooth.png",exportimage];
AbsoluteTime[]-t
17.755160

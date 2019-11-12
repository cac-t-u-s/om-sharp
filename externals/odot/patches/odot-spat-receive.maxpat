{
	"patcher" : 	{
		"fileversion" : 1,
		"appversion" : 		{
			"major" : 8,
			"minor" : 0,
			"revision" : 4,
			"architecture" : "x64",
			"modernui" : 1
		}
,
		"classnamespace" : "box",
		"rect" : [ 263.0, 394.0, 926.0, 568.0 ],
		"bglocked" : 0,
		"openinpresentation" : 0,
		"default_fontsize" : 12.0,
		"default_fontface" : 0,
		"default_fontname" : "Arial",
		"gridonopen" : 1,
		"gridsize" : [ 15.0, 15.0 ],
		"gridsnaponopen" : 1,
		"objectsnaponopen" : 1,
		"statusbarvisible" : 2,
		"toolbarvisible" : 1,
		"lefttoolbarpinned" : 0,
		"toptoolbarpinned" : 0,
		"righttoolbarpinned" : 0,
		"bottomtoolbarpinned" : 0,
		"toolbars_unpinned_last_save" : 0,
		"tallnewobj" : 0,
		"boxanimatetime" : 200,
		"enablehscroll" : 1,
		"enablevscroll" : 1,
		"devicewidth" : 0.0,
		"description" : "",
		"digest" : "",
		"tags" : "",
		"style" : "",
		"subpatcher_template" : "",
		"boxes" : [ 			{
				"box" : 				{
					"id" : "obj-5",
					"linecount" : 2,
					"maxclass" : "comment",
					"numinlets" : 1,
					"numoutlets" : 0,
					"patching_rect" : [ 82.0, 63.5, 150.0, 33.0 ],
					"text" : "click to switch between distribution functions"
				}

			}
, 			{
				"box" : 				{
					"fontface" : 0,
					"fontsize" : 12.0,
					"id" : "obj-26",
					"linecount" : 2,
					"maxclass" : "o.expr.codebox",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "FullPacket", "FullPacket" ],
					"patching_rect" : [ 189.0, 170.0, 146.0, 30.0 ],
					"text" : "eval(/cleanup)\n\n",
					"textcolor" : [ 0.0, 0.0, 0.0, 1.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontface" : 0,
					"fontsize" : 12.0,
					"id" : "obj-24",
					"maxclass" : "o.expr.codebox",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "FullPacket", "FullPacket" ],
					"patching_rect" : [ 345.0, 113.0, 150.0, 32.0 ],
					"text" : "eval(/cloudfun3)",
					"textcolor" : [ 0.0, 0.0, 0.0, 1.0 ]
				}

			}
, 			{
				"box" : 				{
					"id" : "obj-23",
					"maxclass" : "gswitch2",
					"numinlets" : 2,
					"numoutlets" : 3,
					"outlettype" : [ "", "", "" ],
					"outputs" : 3,
					"parameter_enable" : 0,
					"patching_rect" : [ 33.0, 63.5, 39.0, 32.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontface" : 0,
					"fontsize" : 12.0,
					"id" : "obj-3",
					"maxclass" : "o.expr.codebox",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "FullPacket", "FullPacket" ],
					"patching_rect" : [ 189.0, 113.0, 150.0, 32.0 ],
					"text" : "eval(/cloudfun2)",
					"textcolor" : [ 0.0, 0.0, 0.0, 1.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontface" : 0,
					"fontsize" : 12.0,
					"id" : "obj-9",
					"linecount" : 15,
					"maxclass" : "o.display",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 33.0, 239.0, 358.0, 224.0 ],
					"text" : "/source/1/xyz : [-0.478691, 3.49776, 0],\n/source/2/xyz : [-0.436507, 3.83464, 0],\n/source/3/xyz : [-0.478255, 4.04209, 0],\n/source/4/xyz : [-0.613595, 4.08404, 0],\n/source/5/xyz : [-0.822423, 3.95321, 0],\n/source/6/xyz : [-1.05777, 3.67233, 0],\n/source/7/xyz : [-1.25582, 3.29023, 0],\n/source/8/xyz : [-1.351, 2.87332, 0],\n/source/9/xyz : [-1.29274, 2.49408, 0],\n/source/10/xyz : [-1.06026, 2.21842, 0],\n/source/11/xyz : [-0.671483, 2.09426, 0],\n/source/12/xyz : [-0.183862, 2.14319, 0],\n/source/13/xyz : [0.313855, 2.35668, 0],\n/source/14/xyz : [0.717742, 2.69765, 0],\n/source/15/xyz : [0.929788, 3.10681, 0]",
					"textcolor" : [ 1.0, 1.0, 1.0, 1.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontface" : 0,
					"fontsize" : 12.0,
					"id" : "obj-8",
					"linecount" : 32,
					"maxclass" : "o.display",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 573.0, 52.0, 274.0, 455.0 ],
					"text" : "/cleanup : \"delete(/x), delete(/y), delete(/n), delete(/spread), delete(/cloudfun1), delete(/cloudfun2), delete(/cloudfun3), delete(/cleanup)\",\n/cloudfun1 : \"map(lambda([i], \n\tassign(\\\"/source/\\\" + string(i) + \\\"/xyz\\\", [/x + ( /spread * i * 0.1 * cos(6.3 / /n * i)), /y +  ( /spread * sin(6.3 / /n * i)), 0])),\n\taseq(1, /n))\n\",\n/cloudfun2 : \"map(lambda([i], \n\tassign(\\\"/source/\\\" + string(i) + \\\"/xyz\\\", [/x + ( /spread * cos(6.3 / /n * i)), /y +  ( /spread * sin(6.3 / /n * i)), 0])),\n\taseq(1, /n))\n\",\n/cloudfun3 : \"map(lambda([i], \n\tassign(\\\"/source/\\\" + string(i) + \\\"/xyz\\\", [/x + ( /spread * i * 0.1 * cos(6.3 / /n * i * 3)), /y +  ( /spread * i * 0.1 * sin(6.3 / /n * i * 3)), 0])),\n\taseq(1, /n))\n\",\n/x : -0.57,\n/y : 3.09,\n/spread : 1,\n/n : 15",
					"textcolor" : [ 1.0, 1.0, 1.0, 1.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontface" : 0,
					"fontsize" : 12.0,
					"id" : "obj-4",
					"maxclass" : "o.expr.codebox",
					"numinlets" : 1,
					"numoutlets" : 2,
					"outlettype" : [ "FullPacket", "FullPacket" ],
					"patching_rect" : [ 33.0, 113.0, 150.0, 32.0 ],
					"text" : "eval(/cloudfun1)",
					"textcolor" : [ 0.0, 0.0, 0.0, 1.0 ]
				}

			}
, 			{
				"box" : 				{
					"fontname" : "Arial",
					"fontsize" : 12.0,
					"id" : "obj-1",
					"maxclass" : "newobj",
					"numinlets" : 1,
					"numoutlets" : 1,
					"outlettype" : [ "" ],
					"patching_rect" : [ 53.0, 8.0, 135.0, 22.0 ],
					"text" : "udpreceive 3000 cnmat"
				}

			}
 ],
		"lines" : [ 			{
				"patchline" : 				{
					"destination" : [ "obj-23", 1 ],
					"order" : 1,
					"source" : [ "obj-1", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-8", 0 ],
					"midpoints" : [ 62.5, 40.5, 582.5, 40.5 ],
					"order" : 0,
					"source" : [ "obj-1", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-24", 0 ],
					"midpoints" : [ 62.5, 103.75, 354.5, 103.75 ],
					"source" : [ "obj-23", 2 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-3", 0 ],
					"midpoints" : [ 52.5, 103.75, 198.5, 103.75 ],
					"source" : [ "obj-23", 1 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-4", 0 ],
					"source" : [ "obj-23", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-26", 0 ],
					"source" : [ "obj-24", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-9", 0 ],
					"midpoints" : [ 198.5, 207.0, 42.5, 207.0 ],
					"source" : [ "obj-26", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-26", 0 ],
					"midpoints" : [ 198.5, 154.0, 198.5, 154.0 ],
					"source" : [ "obj-3", 0 ]
				}

			}
, 			{
				"patchline" : 				{
					"destination" : [ "obj-26", 0 ],
					"midpoints" : [ 42.5, 154.0, 198.5, 154.0 ],
					"source" : [ "obj-4", 0 ]
				}

			}
 ],
		"dependency_cache" : [ 			{
				"name" : "o.expr.codebox.mxo",
				"type" : "iLaX"
			}
, 			{
				"name" : "o.display.mxo",
				"type" : "iLaX"
			}
 ],
		"autosave" : 0
	}

}

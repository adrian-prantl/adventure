[
 new_object(light,'a clear burning flame', 'It is hot.'),
 new_object(bread,'a loaf of bread','The bread seems extremely durable.'),
 new_object(lighter,'a lighter','A real Zippo.'),
 new_object(stove,'grandmother\'s stove.',
	    'It is made out of shiny white emaille.', [can_be_opened]),

 new_location([living,room], 'A cosy living room.',[kitchen],[lighter]),
 new_location(kitchen,'The kitchen is small.',[[living,room]],[gnome,stove]),
 new_inside(stove, bread),
 new_person(gnome, 'Even for a gnome he seems unusually hairy.'),
 new_inside(inventory, lighter)
]-kitchen
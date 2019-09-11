import React, {useEffect, useState} from 'react';
import {createStyles, makeStyles} from '@material-ui/core/styles';
import List from '@material-ui/core/List';
import ListItem from '@material-ui/core/ListItem';
import ListItemIcon from '@material-ui/core/ListItemIcon';
import ListItemText from '@material-ui/core/ListItemText';
import Divider from '@material-ui/core/Divider';
import DraftsIcon from '@material-ui/icons/Drafts';
import Grid from '@material-ui/core/Grid';
import axios from 'axios';
import DateFnsUtils from "@date-io/date-fns";
import { DatePicker, MuiPickersUtilsProvider} from "@material-ui/pickers";


const useStyles = makeStyles((theme) =>
  createStyles({
    root: {
      flexGrow: 1,
    },
    paper: {
      padding: theme.spacing(2),
      textAlign: 'center',
      color: theme.palette.text.secondary,
    },
  }),
);

function CenteredGrid(classes, acs, isLoading, isError, date, changeDate) {
  return (
    <div className={classes.root}>
      <Grid container spacing={3}>
        <Grid item xs={3}>
          {SimpleList(classes, acs, isLoading, isError)}
        </Grid>
        <Grid item xs={9}>
          <MuiPickersUtilsProvider utils={DateFnsUtils}>
            {StaticDatePicker(date, changeDate)}
          </MuiPickersUtilsProvider>
        </Grid>
      </Grid>
    </div>
  );
}

function SimpleList(classes, acs, isLoading) {
  if (isLoading) {
    return (
      <div className={classes.root}>
        <List component="nav" aria-label="main mailbox folders">
          <ListItem button>
            <ListItemIcon>
              <DraftsIcon />
            </ListItemIcon>
            <ListItemText primary="Connecting to ledger ..." />
          </ListItem>
        </List>
      </div>
    );
  } else {
    return (
      <div className={classes.root}>
        <List component="nav" aria-label="main mailbox folders">
          <ListItem button>
            <ListItemIcon>
              <DraftsIcon />
            </ListItemIcon>
            <ListItemText primary="HolidayRequest Alice" />
          </ListItem>
          <ListItem button>
            <ListItemIcon>
              <DraftsIcon />
            </ListItemIcon>
            <ListItemText primary={acs.length} />
          </ListItem>
        </List>
        <Divider />
      </div>
    );
  }
}

function StaticDatePicker(date, changeDate) {

  // prettier-ignore
  return (
    //<>
      //<DatePicker
        //autoOk
        //variant="static"
        //openTo="year"
        //value={date}
        //onChange={changeDate}
      ///>

      <DatePicker
        autoOk
        orientation="landscape"
        variant="static"
        openTo="date"
        value={date}
        onChange={changeDate}
      />
    //</>
  );
};

function Login(setToken) {
  return (
    <div className="login">
      <img src="img/login.png" alt="Digital Asset Pinacolada!"></img>
      <br />
      <input id="token" type="text"></input>
      <input type="Button" onClick={setToken} value="Login" readOnly></input>
    </div>
  )
}

export default function App() {
  const [acs, setAcs] = useState(null)
  const [token, setToken] = useState("NO_TOKEN")
  const [isLoading, setIsLoading] = useState(false);
  const [date, changeDate] = useState(new Date());
  const classes = useStyles();

  useEffect(() => {
    setIsLoading(true);
    const fetchAcs = async () => {
      try {
        const result = await axios({
          method: 'get',
          url: '/contracts/search',
          headers: {Authorization: 'Bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJsZWRnZXJJZCI6Ik15TGVkZ2VyIiwiYXBwbGljYXRpb25JZCI6ImZvb2JhciIsInBhcnR5IjoiQWxpY2UifQ.4HYfzjlYr1ApUDot0a6a4zB49zS_jrwRUOCkAiPMqo0'},
        });
        console.log(result.data)
        setAcs(result.data.result);
      } catch (error) {
        console.error(error.toString());
      }

      setIsLoading(false);
    }

    if (token !== "NO_TOKEN") fetchAcs();
  }, [token])


  if (token === "NO_TOKEN")
    return (Login(setToken))
  else
    return (CenteredGrid(classes, acs, isLoading, date, changeDate))
}
